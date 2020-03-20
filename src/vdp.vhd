--
--  vdp.vhd
--   VHDL Source of ESE-VDP.
--
--    Copyright (C)2000-2003 Kunihiko Ohnaka
--              All rights reserved.
--
-- JP: 日本語のコメント行は JP:を頭に付ける事にする
--
-- 30th,December,2003 modified by Kazuhiro Tsujikawa
-- JP: 起動時の画面モードをNTSCと VGAのどちらにするかを，外部入力で切替
-- JP: DHClk/DLClkを一時的に復活させた
--
-- 16th,December,2003 modified by Kunihiko Ohnaka
-- JP: 起動時の画面モードをNTSCと VGAのどちらにするかを，vdp_package.vhd
-- JP: 内で定義された定数で切替えるようにした．
--
-- 10th,December,2003 modified by Kunihiko Ohnaka
-- JP: TEXT MODE 2 (SCREEN0 WIDTH80)をサポート．
-- JP: 初の横方向倍解像度モードである．一応将来対応できるように作って
-- JP: きたつもりだったが，少し収まりが悪い部分があり，あまりきれいな
-- JP: 対応になっていない部分もあります．
--
-- 13th,October,2003 modified by Kunihiko Ohnaka
-- JP: ESE-MSX基板では 2S300Eを複数用いる事ができるようにり，VDP単体で
-- JP: 2S300Eや SRAMを占有する事が可能となった．
-- JP: これに伴い以下のような変更を行う．
-- JP: ・VGA出力対応(アップスキャンコンバート)
-- JP: ・SCREEN7,8のタイミングを実機と同じに
--
-- 15th,June,2003 modified by Kunihiko Ohnaka
-- 水平帰線期間割り込みを実装してスペースマンボウを遊べるようにした．
-- GraphicMode3(Screen4)でYライン数が 212ラインにならなかったのを
-- 修正したりした．
-- ただし，スペースマンボウで set adjust機能が動いていないような
-- 感じで，表示がガクガクしてしまう．横方向の同時表示スプライト数も
-- 足りていないように見える．原因不明．
--
-- 15th,June,2003 modified by Kunihiko Ohnaka
-- 長いブランクが空いてしまったが，Spartan-II E + IO基板でスプライトが
-- 表示されるようになった．原因はおそらくコンパイラのバグで，ISE 5.2に
-- バージョンアップしたら表示されるようになった．
-- ついでに，スプライトモード2で横 8枚並ぶようにした(つもり)．
-- その他細かな修正が入っています．
--
-- 15th,July,2002 modified by Kazuhiro Tsujikawa
-- no comment;
--


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use work.vdp_package.all;

entity vdp is
  port(
    -- VDP clock ... 21.477MHz
    clk21m  : in std_logic;
    reset   : in std_logic;
    req     : in std_logic;
    ack     : out std_logic;
    wrt     : in std_logic;
    adr     : in std_logic_vector(15 downto 0);
    dbi     : out std_logic_vector(7 downto 0);
    dbo     : in std_logic_vector(7 downto 0);

    int_n   : out std_logic;

    pRamOe_n: out std_logic;
    pRamWe_n: out std_logic;
    pRamAdr : out std_logic_vector(16 downto 0);
    pRamDat : inout std_logic_vector(7 downto 0);

    -- Video Output
    pVideoR : out std_logic_vector( 5 downto 0);
    pVideoG : out std_logic_vector( 5 downto 0);
    pVideoB : out std_logic_vector( 5 downto 0);

    pVideoHS_n : out std_logic;
    pVideoVS_n : out std_logic;
    pVideoCS_n : out std_logic;

    pVideoDHClk : out std_logic;
    pVideoDLClk : out std_logic;

    -- CXA1645(RGB->NTSC encoder) signals
    pVideoSC : out std_logic;
    pVideoSYNC : out std_logic;

    -- Display resolution (0=15kHz, 1=31kHz)
    DispReso : in  std_logic
    );
end vdp;

architecture rtl of vdp is
  
  
-- H counter
  signal h_counter : std_logic_vector(10 downto 0);
-- V counter
  signal v_counter : std_logic_vector(10 downto 0);

-- display start position ( when adjust=(0,0) )
  constant OFFSET_X : std_logic_vector( 6 downto 0) := "0110110";       -- = 220/4;
  constant OFFSET_Y : std_logic_vector( 6 downto 0) := "0101110";       -- = 3+3+13+26+1 = 46
  constant OFFSET_Y_212 : std_logic_vector( 6 downto 0) := "0100100";   -- = 3+3+13+16+1 = 36

--    signal adjust_x : std_logic_vector( 6 downto 0);
--    signal adjust_y : std_logic_vector( 6 downto 0);

-- dot state register
  signal dotState : std_logic_vector( 1 downto 0);
  signal dotResetState : std_logic_vector( 1 downto 0);

-- display field signal  
  signal field  : std_logic;

-- sync state register
  type typsstate is (sstate_A, sstate_B, sstate_C, sstate_D);
  signal sstate : typsstate;

--@ constant sstate_A : std_logic_vector := "00";
--@ constant sstate_B : std_logic_vector := "01";
--@ constant sstate_C : std_logic_vector := "10";
--@ constant sstate_D : std_logic_vector := "11";

  -- for vsync interrupt
  signal vsyncInt_n : std_logic;
  signal vsyncIntReq : std_logic;
  signal vsyncIntAck : std_logic;
  signal dVideoVS_n : std_logic;

  -- for hsync interrupt
  signal hsyncInt_n : std_logic;
  signal hsyncIntReq : std_logic;
  signal hsyncIntAck : std_logic;
  signal dVideoHS_n : std_logic;
  
  -- display area flags
  signal window_x :std_logic;
  signal window_y :std_logic;
  signal window :std_logic;
  signal preWindow_x :std_logic;
  signal preWindow_y :std_logic;
  signal preWindow :std_logic;
  -- for sprites
  signal spwindow_ec :std_logic;
  signal spwindow :std_logic;
  signal spwindow_x :std_logic;
  signal spwindow_y :std_logic;
  signal spwindow_ecx :std_logic;
  -- for text mode
  signal twindow : std_logic;
  signal twindow_y : std_logic;
  signal twindow_x : std_logic;
  -- for graphic mode 1,2,3
  signal g123window : std_logic;
  signal g123window_y : std_logic;
  signal g123window_x : std_logic;
  -- for frame zone
  signal bwindow_x :std_logic;
  signal bwindow_y :std_logic;
  signal bwindow :std_logic;

-- dot counter
  signal dotCounter_x : std_logic_vector( 8 downto 0);
  signal dotCounter_y : std_logic_vector( 7 downto 0);
  -- 垂直スクロールレジスタの影響を受けないカウンタ
  signal dotCounter_yp : std_logic_vector( 7 downto 0);
-- dot counter - 8 ( fifo read addr )
  signal preDotCounter_x : std_logic_vector( 8 downto 0);
--  signal preDotCounter_y : std_logic_vector( 7 downto 0);
  signal preDotCounter_y : std_logic_vector( 8 downto 0);
  -- 垂直スクロールレジスタの影響を受けないカウンタ
--  signal preDotCounter_yp : std_logic_vector( 7 downto 0);
  signal preDotCounter_yp : std_logic_vector( 8 downto 0);
  signal preDotCounter_x_end : std_logic;

  signal VramReadFreeFlag : std_logic;

-- 3.58MHz generator
  signal cpuClockCounter :std_logic_vector( 2 downto 0);

-- VDP register access
  signal VdpP1Is1stByte : std_logic;
  signal VdpP2Is1stByte : std_logic;
  signal VdpP0Data : std_logic_vector( 7 downto 0);
  signal VdpP1Data : std_logic_vector( 7 downto 0);
  signal VdpRegPtr : std_logic_vector( 5 downto 0);
  signal VdpRegWrPulse : std_logic;
  signal VdpVramAccessAddr : std_logic_vector( 16 downto 0);
  signal VdpVramAccessData : std_logic_vector( 7 downto 0);
  signal VdpVramAccessAddrTmp : std_logic_vector( 16 downto 0);
  signal VdpVramAddrSetReq : std_logic;
  signal VdpVramAddrSetAck : std_logic;
  signal VdpVramAccessRw : std_logic;
  signal VdpVramWrReq : std_logic;
  signal VdpVramWrAck : std_logic;
  signal VdpVramReading : std_logic;
  signal VdpVramRdData : std_logic_vector(7 downto 0);
  signal VdpVramRdReq : std_logic;
  signal VdpVramRdAck : std_logic;
  signal dispModeVGA : std_logic;
  signal VdpR0DispNum : std_logic_vector(3 downto 1);
  signal VdpR0HSyncIntEn : std_logic;
  signal VdpR1DispMode : std_logic_vector(1 downto 0);
  signal VdpR1SpSize : std_logic;
  signal VdpR1SpZoom : std_logic;
  signal VdpR1VSyncIntEn : std_logic;
  signal VdpR1DispOn : std_logic;
  signal VdpR2PtnNameTblBaseAddr : std_logic_vector( 6 downto 0);
  signal VdpR4PtnGeneTblBaseAddr : std_logic_vector( 5 downto 0);
  signal VdpR10R3ColorTblBaseAddr : std_logic_vector( 10 downto 0);
  signal VdpR11R5SpAttrTblBaseAddr : std_logic_vector( 9 downto 0);
  signal VdpR6SpPtnGeneTblBaseAddr : std_logic_vector( 5 downto 0);
  signal VdpR7FrameColor : std_logic_vector( 7 downto 0);
  signal VdpR8SpOff : std_logic;
  signal VdpR8Color0On : std_logic;
  signal VdpR9InterlaceMode : std_logic;
  signal VdpR9TwoPageMode : std_logic;
  signal VdpR9YDots : std_logic;
  signal VdpR15StatusRegNum : std_logic_vector( 3 downto 0);
  signal VdpR16PalNum : std_logic_vector( 3 downto 0);
  signal VdpR17RegNum : std_logic_vector( 5 downto 0);
  signal VdpR17IncRegNum : std_logic;
  signal VdpR18Adjust : std_logic_vector( 7 downto 0);
  signal VdpR19HSyncIntLine : std_logic_vector( 7 downto 0);
  signal VdpR23VStartLine : std_logic_vector( 7 downto 0);
  signal VdpModeText1 : std_logic;      -- text mode 1    (SCREEN0)
  signal VdpModeText2 : std_logic;      -- text mode 2    (SCREEN0 width 80)
  signal VdpModeGraphic1 : std_logic;   -- graphic mode 1 (SCREEN1)
  signal VdpModeGraphic2 : std_logic;   -- graphic mode 2 (SCREEN2)
  signal VdpModeGraphic3 : std_logic;   -- graphic mode 2 (SCREEN4)
  signal VdpModeGraphic4 : std_logic;   -- graphic mode 4 (SCREEN5)
  signal VdpModeGraphic5 : std_logic;   -- graphic mode 5 (SCREEN6)
  signal VdpModeGraphic6 : std_logic;   -- graphic mode 6 (SCREEN7)
  signal VdpModeGraphic7 : std_logic;   -- graphic mode 7 (SCREEN8)
--    constant VdpModeGraphic7 : std_logic := '0';

-- Color Code
  signal colorCode : std_logic_vector( 7 downto 0);
  signal colorR : std_logic_vector( 2 downto 0);
  signal colorG : std_logic_vector( 2 downto 0);
  signal colorB : std_logic_vector( 2 downto 0);

  -- for text 1
  signal TXDotCounter_x : std_logic_vector( 2 downto 0);
  signal TXCharCounter_x : std_logic_vector( 9 downto 0);
  signal T1CharCounter_y : std_logic_vector( 9 downto 0);
  signal T1PtnNum : std_logic_vector( 7 downto 0);
  signal T1Pattern : std_logic_vector( 7 downto 0);
  signal T1ColorCode : std_logic_vector( 3 downto 0);

  signal T2CharCounter_y : std_logic_vector(11 downto 0);
  signal T2PtnNum : std_logic_vector( 7 downto 0);
  signal T2Pattern : std_logic_vector( 7 downto 0);
  signal T2ColorCode : std_logic_vector( 3 downto 0);


-- for graphic 1,2,3
  signal G1PtnNum : std_logic_vector( 7 downto 0);
  signal G1PPattern : std_logic_vector( 7 downto 0);
  signal G1Pattern : std_logic_vector( 7 downto 0);
  signal G1Color : std_logic_vector( 7 downto 0);
  signal G1ColorCode : std_logic_vector( 3 downto 0);

  -- for graphic 4
  signal G4ColorCode : std_logic_vector( 3 downto 0);
  -- for graphic 7
  signal G7ColorCode : std_logic_vector( 7 downto 0);
  
  -- sprite
  signal SpPreReading : std_logic;

  type typSpPreReadState is (spstate_idle, spstate_yread, spstate_xread, spstate_ptnnumread,
                             spstate_ptnread1, spstate_ptnread2, spstate_colorread);
  signal SpPreReadState : typSpPreReadState;

  signal SpMode2 : std_logic;
  signal SpPreReadCounter : std_logic_vector( 4 downto 0);   -- 0 - 31
  signal SpPreReadCounter2 : std_logic_vector( 2 downto 0);  -- 0 - 7
--    signal SpPreReadCounter2 : std_logic_vector( 3 downto 0);  -- JP: 1ビット増やしてみる
  signal SpPreReadPtnNum : std_logic_vector( 7 downto 0);
  signal SpY : std_logic_vector( 7 downto 0);
  signal SpPreReadY : std_logic_vector( 7 downto 0);
  signal spColorCode : std_logic_vector( 2 downto 0);
  signal spColorCode_3 : std_logic;
  signal spColorIsTransparent : std_logic;
  signal SpDispEnd : std_logic;

  constant SpMode1_nSprites: integer := 4;
  constant SpMode2_nSprites: integer := 8;

  subtype spPatternType is std_logic_vector(16 downto 0);
  subtype spXType is std_logic_vector(8 downto 0);
  subtype spColorType is std_logic_vector(3 downto 0);
  
  type spbitVec is array( 0 to SpMode2_nSprites-1 ) of std_logic;
  type sppatternVec is array( 0 to SpMode2_nSprites-1 ) of spPatternType;
  type spxVec is array( 0 to SpMode2_nSprites-1 ) of spXType;
  type spcolorVec is array( 0 to SpMode2_nSprites-1 ) of spColorType;

  signal SpColorIn : spbitVec;
  signal SpPattern : sppatternVec;
  signal SpX : spxVec;
  signal SpColor : spcolorVec;
  -- JP: 連続したスプライト番号のスプライトが同一ラインに並んでいるか
  -- JP: どうかを判定するためのフラグ
  signal SpReadIsContinuous : std_logic;
  signal SpCC : spbitVec;
  signal SpEC : spbitVec;
  signal SpIC : spbitVec;

  signal fifoAddr : std_logic_vector( 7 downto 0);
  signal fifoAddr_in : std_logic_vector( 3 downto 0);
  signal fifoAddr_out : std_logic_vector( 3 downto 0);
  signal fifoWe : std_logic;
  signal fifoIn : std_logic;
  signal fifoData_in : std_logic_vector( 7 downto 0);
  signal fifoData_out : std_logic_vector( 7 downto 0);
  signal fifoFull : std_logic;

  -- palette registers
  signal paletteAddr : std_logic_vector( 7 downto 0);
  signal paletteAddr_out : std_logic_vector( 3 downto 0);
  signal paletteWeRB : std_logic;
  signal paletteWeG : std_logic;
  signal paletteInRB : std_logic;
  signal paletteInG : std_logic;
  signal paletteData_in : std_logic_vector( 7 downto 0);
  signal paletteDataRB_out : std_logic_vector( 7 downto 0);
  signal paletteDataG_out : std_logic_vector( 7 downto 0);

  signal paletteWrTemp : std_logic_vector( 7 downto 0);
  signal paletteWrNum : std_logic_vector( 3 downto 0);
  signal paletteWrReqRB : std_logic;
  signal paletteWrAckRB : std_logic;
  signal paletteWrReqG : std_logic;
  signal paletteWrAckG : std_logic;

  signal iVideoR : std_logic_vector( 5 downto 0);
  signal iVideoG : std_logic_vector( 5 downto 0);
  signal iVideoB : std_logic_vector( 5 downto 0);
  signal iVideoHS_n : std_logic;
  signal iVideoVS_n : std_logic;

  signal iVideoR_ntsc : std_logic_vector( 5 downto 0);
  signal iVideoG_ntsc : std_logic_vector( 5 downto 0);
  signal iVideoB_ntsc : std_logic_vector( 5 downto 0);
  signal iVideoHS_n_ntsc : std_logic;
  signal iVideoVS_n_ntsc : std_logic;

  signal iVideoR_vga : std_logic_vector( 5 downto 0);
  signal iVideoG_vga : std_logic_vector( 5 downto 0);
  signal iVideoB_vga : std_logic_vector( 5 downto 0);
  signal iVideoHS_n_vga : std_logic;
  signal iVideoVS_n_vga : std_logic;
  
begin
  ----------------------------------------------------------------
  -- Display Components
  ----------------------------------------------------------------
  ntsc1 : ntsc port map( clk21m,
                         reset,
                         iVideoR,
                         iVideoG,
                         iVideoB,
                         iVideoHS_n,
                         iVideoVS_n,
                         iVideoR_ntsc,
                         iVideoG_ntsc,
                         iVideoB_ntsc,
                         iVideoHS_n_ntsc,
                         iVideoVS_n_ntsc);

  vga1 : vga port map( clk21m,
                       reset,
                       iVideoR,
                       iVideoG,
                       iVideoB,
                       h_counter,
                       v_counter,
                       iVideoR_vga,
                       iVideoG_vga,
                       iVideoB_vga,
                       iVideoHS_n_vga,
                       iVideoVS_n_vga);
  
  -- JP: 独自に拡張したモードレジスタ( VDP #4ポート)で 画面モードを変える
  pVideoR <= iVideoR_ntsc when dispModeVGA = '0' else iVideoR_vga;
  pVideoG <= iVideoG_ntsc when dispModeVGA = '0' else iVideoG_vga; 
  pVideoB <= iVideoB_ntsc when dispModeVGA = '0' else iVideoB_vga;
  -- H Sync signal
  pVideoHS_n <= iVideoHS_n_ntsc when dispModeVGA = '0' else iVideoHS_n_vga;
  -- V Sync signal
  pVideoVS_n <= iVideoVS_n_ntsc when dispModeVGA = '0' else iVideoVS_n_vga;

  
  pVideoSYNC <= not (iVideoHS_n_ntsc xor iVideoVS_n_ntsc) when dispModeVGA = '0'
                else not (iVideoHS_n_vga xor iVideoVS_n_vga);
  
  -- JP: 以下の信号は画面モードに関わらず直接出力する
  pVideoCS_n <= not (iVideoHS_n xor iVideoVS_n);
--  pVideoSYNC <= not (iVideoHS_n xor iVideoVS_n);
  pVideoSC <= cpuClockCounter(2);


  
  ----------------------------------------------------------------
  -- 16byte FIFO control
  ----------------------------------------------------------------
  fifoAddr <= ( "0000" & fifoAddr_in ) when (fifoIn = '1') else
              ( "0000" & fifoAddr_out );
  fifoData_in <= pRamDat;
  fifoWe   <= '1' when fifoIn = '1' else '0';
  fifoFull <= '1' when fifoAddr_in+1 = fifoAddr_out else '0';

  fifoMem : ram port map(fifoAddr, clk21m, fifoWe, fifoData_in, fifoData_out);

  ----------------------------------------------------------------
  -- Palette Register control R and B
  ----------------------------------------------------------------
  paletteAddr <= ( "0000" & paletteWrNum ) when (paletteInRB = '1' or paletteInG = '1') else
                 ( "0000" & paletteAddr_out );
  paletteData_in <= paletteWrTemp;
  paletteWeRB  <= '1' when paletteInRB = '1' else '0';
  paletteWeG   <= '1' when paletteInG  = '1' else '0';

  paletteMemRB : ram port map(paletteAddr, clk21m, paletteWeRB, paletteData_in, paletteDataRB_out);
  paletteMemG  : ram port map(paletteAddr, clk21m, paletteWeG,  paletteData_in, paletteDataG_out);

  -- VSYNC Interrupt
  vsyncInt_n <= '0' when (vsyncIntReq /= vsyncIntAck)
                else '1';
  -- HSYNC Interrupt
  hsyncInt_n <= '0' when (hsyncIntReq /= hsyncIntAck)
                else '1';

  int_n <= '0' when (vsyncInt_n = '0') or (hsyncInt_n = '0')
           else 'Z';

  -- VDP Mode
  VdpModeText1    <= '1' when (VdpR0DispNum = "000" and VdpR1DispMode = "10") else '0';
  VdpModeText2    <= '1' when (VdpR0DispNum = "010" and VdpR1DispMode = "10") else '0';
  VdpModeGraphic1 <= '1' when (VdpR0DispNum = "000" and VdpR1DispMode = "00") else '0';
  VdpModeGraphic2 <= '1' when (VdpR0DispNum = "001" and VdpR1DispMode = "00") else '0';
  VdpModeGraphic3 <= '1' when (VdpR0DispNum = "010" and VdpR1DispMode = "00") else '0';
  VdpModeGraphic4 <= '1' when (VdpR0DispNum = "011" and VdpR1DispMode = "00") else '0';
  VdpModeGraphic5 <= '1' when (VdpR0DispNum = "100" and VdpR1DispMode = "00") else '0';
  VdpModeGraphic6 <= '1' when (VdpR0DispNum = "101" and VdpR1DispMode = "00") else '0';
  VdpModeGraphic7 <= '1' when (VdpR0DispNum = "111" and VdpR1DispMode = "00") else '0';
  SpMode2 <= '1' when (VdpModeGraphic3 = '1' or
                       VdpModeGraphic4 = '1' or
                       VdpModeGraphic5 = '1' or
                       VdpModeGraphic6 = '1' or
                       VdpModeGraphic7 = '1' ) else '0';

  process( clk21m, reset )
  begin
    if (reset = '1') then
      ack <= '0';
      h_counter <= (others => '0');
      v_counter <= (others => '0');
      iVideoHS_n <= '1';
      iVideoVS_n <= '1';
      dVideoVS_n <= '1';
      cpuClockCounter <= (others => '0');
      sstate <= sstate_A;
      field <= '0';
      vsyncIntReq <= '0';
      hsyncIntReq <= '0';
    elsif (clk21m'event and clk21m = '1') then
      ack <= req;
      -- 3.58MHz generator
      case cpuClockCounter is
        when "000" => cpuClockCounter <= "001";
        when "001" => cpuClockCounter <= "011";
        when "011" => cpuClockCounter <= "111";
        when "111" => cpuClockCounter <= "110";
        when "110" => cpuClockCounter <= "100";
        when "100" => cpuClockCounter <= "000";
        when others => cpuClockCounter <= "000";
      end case;

      if( h_counter = CLOCKS_PER_LINE-1 ) then
        h_counter <= (others => '0' );
      else
        h_counter <= h_counter + 1;
      end if;

      if( (h_counter = (CLOCKS_PER_LINE/2) -1) or (h_counter = CLOCKS_PER_LINE-1) ) then
        -- 524 lines * 2 = 1048
        -- 525 lines * 2 = 1050
--        if( v_counter = 1049 ) then
        if((v_counter = 1047 and VdpR9InterlaceMode = '0') or
           (v_counter = 1049 and VdpR9InterlaceMode = '1')) then
          if(h_counter = CLOCKS_PER_LINE-1) then
            v_counter <= (others => '0');
          end if;
        else 
          v_counter <= v_counter + 1;
        end if;
      end if;

      if( (v_counter = 0) or
          (v_counter = 12) or
          ((v_counter = 524) and VdpR9InterlaceMode = '0') or
          ((v_counter = 525) and VdpR9InterlaceMode = '1') or
          ((v_counter = 524 + 12) and VdpR9InterlaceMode = '0') or
          ((v_counter = 525 + 12) and VdpR9InterlaceMode = '1') )then
        sstate <= sstate_A;
      elsif( (v_counter = 6) or
             ((v_counter = 524+6) and VdpR9InterlaceMode = '0') or
             ((v_counter = 525+6) and VdpR9InterlaceMode = '1') )then
        sstate <= sstate_B;
      elsif( (v_counter = 18) or
             ((v_counter = 524+18) and VdpR9InterlaceMode = '0') or
             ((v_counter = 525+18) and VdpR9InterlaceMode = '1') )then
        sstate <= sstate_C;
      end if;

-- generate field signal
      if( (v_counter = 524 and VdpR9InterlaceMode = '0') or
          (v_counter = 525 and VdpR9InterlaceMode = '1') ) then
        field <= '1';
      elsif( v_counter = 0 ) then
        field <= '0';
      end if;

-- generate H sync pulse
      if( sstate = sstate_A ) then
        if( (h_counter = 1) or (h_counter = CLOCKS_PER_LINE/2+1) ) then
          iVideoHS_n <= '0';             -- pulse on
        elsif( (h_counter = 51) or (h_counter = CLOCKS_PER_LINE/2+51) ) then
          iVideoHS_n <= '1';             -- pulse off
        end if;
      elsif( sstate = sstate_B ) then
        if( (h_counter = CLOCKS_PER_LINE  -100+1) or
            (h_counter = CLOCKS_PER_LINE/2-100+1) ) then
          iVideoHS_n <= '0';             -- pulse on
        elsif( (h_counter =                   1) or
               (h_counter = CLOCKS_PER_LINE/2+1) ) then
          iVideoHS_n <= '1';             -- pulse off
        end if;
      elsif( sstate = sstate_C ) then
        if( h_counter = 1 ) then
          iVideoHS_n <= '0';             -- pulse on
        elsif( h_counter = 101 ) then
          iVideoHS_n <= '1';             -- pulse off
        end if;
      end if;

-- generate V sync pulse
      if( sstate = sstate_B ) then
        iVideoVS_n <= '0';
      else
        iVideoVS_n <= '1';
      end if;   

      
-- V Sync Interrupt Request
--      dVideoVS_n <= iVideoVS_n;
--      if( dVideoVS_n = '1' and iVideoVS_n = '0' ) then
--        -- falling edge
--        if( VdpR1VSyncIntEn = '1' ) then
--          vsyncIntReq <= not vsyncIntAck;
--        end if;
--      end if;
      dVideoVS_n <= bwindow_y;
      if( dVideoVS_n = '1' and bwindow_y = '0' ) then
        -- falling edge
        if( VdpR1VSyncIntEn = '1' ) then
--          vsyncIntReq <= not vsyncIntAck;
          vsyncIntReq <= not vsyncIntReq;
        end if;
      end if;

      -- H Sync Interrupt Request
      -- JP: 水平帰線期間割り込みのかかるラインの判定のしかたが
      -- JP: 実機と違うかも．適当な実装．
      -- JP: R19で指定している"ライン番号"は，垂直スクロールレジスタの
      -- JP: 影響を受けるようである.
--      dVideoHS_n <= iVideoHS_n;
--      if( dVideoHS_n = '1' and iVideoHS_n = '0' ) then
--        -- falling edge
--        if( (VdpR0HSyncIntEn = '1') and
--            (VdpR19HSyncIntLine = preDotCounter_y) ) then
--          hsyncIntReq <= not hsyncIntAck;
--        end if;
--      end if;

--      if( (h_counter = CLOCKS_PER_LINE-1) and ( preWindow_y = '1') ) then
      if( (h_counter = CLOCKS_PER_LINE-1) ) then
        if( (VdpR0HSyncIntEn = '1') and
            (VdpR19HSyncIntLine = preDotCounter_y(7 downto 0)) ) then
--          hsyncIntReq <= not hsyncIntAck;
          hsyncIntReq <= not hsyncIntReq;
        end if;
      end if;
        
    end if;

  end process;

-- generate preWindow, window, g123window, spwindow
  preWindow <= (preWindow_x and preWindow_y);
  window <= (window_x and window_y);
  g123window <= (g123window_x and g123window_y);
  twindow <= (twindow_x and twindow_y);


  spwindow_ec <= spwindow_ecx and spwindow_y;
  spwindow <= spwindow_x and spwindow_y;

  -- JP: preDotCounter_x が最大値になったら1になるフラグ
  -- JP: SCREEN5(Graphic4)は横128バイト読み終わったらライン終了
  -- JP: SCREEN4以前は FIFOを使わないのでpreDotCounter_xが 255まで上がる
  -- JP: (汚い実装)
  preDotCounter_x_end <= '1' when ( ((VdpModeGraphic4 = '1') and (preDotCounter_x = "001111111")) or
                                    ((VdpModeGraphic4 = '0') and (preDotCounter_x = "011111111")) )
                         else '0';
  
  process( clk21m, reset )
    variable adjust_x : std_logic_vector(6 downto 0);
    variable adjust_y : std_logic_vector(6 downto 0);
  begin
    if (reset = '1') then
      dotCounter_x <= (others =>'0');
      dotCounter_y <= (others =>'0');
      dotCounter_yp <= (others =>'0');
      preDotCounter_x <= (others =>'0');
      preDotCounter_y <= (others =>'0');
      preDotCounter_yp <= (others =>'0');
      window_x <= '0';
      window_y <= '0';
      preWindow_x <= '0';
      preWindow_y <= '0';
      bwindow <= '0';
      bwindow_x <= '0';
      bwindow_y <= '0';
      spwindow_x <= '0';
      spwindow_y <= '0';
      spwindow_ecx <= '0';
      g123window_y <= '0';
      g123window_x <= '0';
      twindow_y <= '0';
      twindow_x <= '0';
      VramReadFreeFlag <= '0';
      TXCharCounter_x <= (others => '0');
      T1CharCounter_y <= (others => '0');
      T2CharCounter_y <= (others => '0');
      TXDotCounter_x <= (others => '0');
    elsif (clk21m'event and clk21m = '1') then
      -- adjust
      adjust_x := OFFSET_X - ( VdpR18Adjust(3) & VdpR18Adjust(3) & VdpR18Adjust(3) & VdpR18Adjust(3 downto 0) );
      if( VdpR9YDots = '0' ) then
        adjust_y := OFFSET_Y - ( VdpR18Adjust(7) & VdpR18Adjust(7) & VdpR18Adjust(7) & VdpR18Adjust(7 downto 4) );
      else
        adjust_y := OFFSET_Y_212 - ( VdpR18Adjust(7) & VdpR18Adjust(7) & VdpR18Adjust(7) & VdpR18Adjust(7 downto 4) );
      end if;


      if( h_counter = ("00" & adjust_x & "10" ) ) then
        preWindow_x <= '1';
        g123window_x <= '1';
      elsif( (h_counter( 1 downto 0) = "10") and ( preDotCounter_x_end = '1' ) ) then
        preWindow_x <= '0';
        g123window_x <= '0';
      end if;

      if( (v_counter = ("0000" & (adjust_y+1) & '0') ) or
          ((v_counter = 524+("0000" & (adjust_y+1) & '0')) and VdpR9InterlaceMode='0') or
          ((v_counter = 525+("0000" & (adjust_y+1) & '0')) and VdpR9InterlaceMode='1')) then
        preWindow_y <= '1';
        window_y <= '1';
        g123window_y <= '1';
        twindow_y <= '1';
      elsif( (VdpR9YDots = '1') and
             ( (v_counter = ("0000" & (adjust_y+1) & '0')+212*2) or
               ((v_counter = 524+("0000" & (adjust_y+1) & '0')+212*2) and VdpR9InterlaceMode='0') or
               ((v_counter = 525+("0000" & (adjust_y+1) & '0')+212*2) and VdpR9InterlaceMode='1') ) ) then
        -- JP: 画面モードにかかわりなく Yドット数変更ビットを有効にして
        -- JP: 良いかどうか確認する必要あり．
        preWindow_y <= '0';
        window_y <= '0';
        g123window_y <= '0';
        twindow_y <= '0';
      elsif( (VdpR9YDots = '0') and
             ( (v_counter = ("0000" & (adjust_y+1) & '0')+192*2) or
               ((v_counter = 524+("0000" & (adjust_y+1) & '0')+192*2) and VdpR9InterlaceMode='0') or
               ((v_counter = 525+("0000" & (adjust_y+1) & '0')+192*2) and VdpR9InterlaceMode='1') ) ) then
        preWindow_y <= '0';
        window_y <= '0';
        g123window_y <= '0';
        twindow_y <= '0';
      end if;

      if( (v_counter = ("0000" & adjust_y & '0') ) or
          ((v_counter = 524+("0000" & adjust_y & '0')) and VdpR9InterlaceMode='0') or
          ((v_counter = 525+("0000" & adjust_y & '0')) and VdpR9InterlaceMode='1')  ) then
        spwindow_y <= '1';
      elsif( (v_counter = ("0000" & adjust_y & '0')+212*2) or
             ((v_counter = 524+("0000" & adjust_y & '0')+212*2) and VdpR9InterlaceMode='0') or
             ((v_counter = 525+("0000" & adjust_y & '0')+212*2) and VdpR9InterlaceMode='1') ) then
        spwindow_y <= '0';
      end if;

-- main window
      if( (h_counter( 1 downto 0) = "10") and ( dotCounter_x = "111111111" ) ) then
        -- when dotCounter_x = -1
        window_x <= '1';
      elsif( (h_counter( 1 downto 0) = "10") and ( dotCounter_x ="011111111" ) ) then
        -- when dotCounter_x = 255
        window_x <= '0';
      end if;

--      if( (h_counter( 1 downto 0) = "10") and ( preDotCounter_x = 7- ) and (window_y = '1') ) then
--      elsif( (h_counter( 1 downto 0) = "10") and ( preDotCounter_x = 255 ) ) then
--      end if;
      
      if( h_counter = ("00" & ( adjust_x - 24 -2) & "10" ) ) then
        spwindow_ecx <= '1';
      elsif( h_counter = ("00" & ( adjust_x + 8 -2) & "10" ) ) then
        spwindow_x <= '1';
      elsif( (h_counter( 1 downto 0) = "10") and ( preDotCounter_x_end = '1' ) ) then
        spwindow_x <= '0';
        spwindow_ecx <= '0';
      end if;

-- JP: 16回に 1回 vram アクセス用に vramバスを開放するために、FIFOを用いる
-- JP: 
-- JP:
-- JP:
-- JP:
      -- JP: R23の変更が即座に反映されるようにした. 
      dotCounter_y <= dotCounter_yp + VdpR23VStartLine;

      if( h_counter = ("00" & adjust_x & "10") ) then
        preDotCounter_x <= (others =>'0');
        VramReadFreeFlag <= '0';
        if( v_counter = ("000" & adjust_y & '0') ) then
          dotCounter_yp <= (others => '1');  -- -1
        elsif( ((v_counter = ("000" & adjust_y & '0')+524) and VdpR9InterlaceMode='0') or
               ((v_counter = ("000" & adjust_y & '0')+525) and VdpR9InterlaceMode='1') ) then
          dotCounter_yp <= (others => '1');  -- -1
        else
          dotCounter_yp <= dotCounter_yp + 1;
        end if;
      elsif( (h_counter( 1 downto 0) = "10") and
             (VramReadFreeFlag = '0') and
             (fifoFull = '0' ) ) then
        -- JP: ReadFreeFlagが立っておらず、FIFOが FULLでないなら、
        -- JP: 1"バイト"読む。これは SCREEN5でも8でもいっしょ。
        -- JP: (1ドット表示する間に 1バイト読む)
        preDotCounter_x <= preDotCounter_x + 1;
        if( preDotCounter_x( 3 downto 0 ) = "1111" ) then
          VramReadFreeFlag <= '1';
        end if;
      elsif( (h_counter( 1 downto 0) = "10") and (VramReadFreeFlag = '1') ) then
        VramReadFreeFlag <= '0';
      end if;

      -- dot counter
      if( h_counter = ("00" & adjust_x & "10") ) then
        dotCounter_x <= "111111000";      -- -8
      elsif( h_counter( 1 downto 0) = "10") then 
        dotCounter_x <= dotCounter_x + 1;
      end if;

      -- JP: R23の変更が即座に反映されるようにした. 
      preDotCounter_y <= preDotCounter_yp + ('0' & VdpR23VStartLine);
      if( h_counter = CLOCKS_PER_LINE-1 ) then
        if( v_counter = ("0000" & adjust_y & '1') ) then
          preDotCounter_yp <= (others => '0');
        elsif( ((v_counter = ("0000" & adjust_y & '1')+524) and VdpR9InterlaceMode='0') or
               ((v_counter = ("0000" & adjust_y & '1')+525) and VdpR9InterlaceMode='1') ) then
          preDotCounter_yp <= (others => '0');
        else
          preDotCounter_yp <= preDotCounter_yp + 1;
        end if;
      end if;

      if( VdpModeText1 = '1') then
        -- counter for Text 1
        if( (h_counter( 1 downto 0) = "10") and ( dotCounter_x = "111111111" ) ) then
          TXDotCounter_x <= "100";          --  4
          TXCharCounter_x <= "1111111111";  -- -1
          if( dotCounter_y = "00000000" ) then
            T1CharCounter_y <= (others => '0');
          elsif( dotCounter_y( 2 downto 0) = "000" ) then
            T1CharCounter_y <= T1CharCounter_y + 40;
          end if;
        elsif( h_counter( 1 downto 0) = "10") then
          if( TXDotCounter_x = "101" ) then
            TXDotCounter_x <= "000";
            TXCharCounter_x <= TXCharCounter_x + 1;
          else
            TXDotCounter_x <= TXDotCounter_x + 1;
          end if;
        end if;
      elsif( VdpModeText2 = '1' ) then
        -- counter for Text 2
        if( (h_counter( 1 downto 0) = "10") and ( dotCounter_x = "111111111" ) ) then
          TXDotCounter_x <= "001";          --  1
          TXCharCounter_x <= "1111111110";  -- -2
          if( dotCounter_y = "00000000" ) then
            T2CharCounter_y <= (others => '0');
          elsif( dotCounter_y( 2 downto 0) = "000" ) then
            T2CharCounter_y <= T2CharCounter_y + 80;
          end if;
        elsif( h_counter( 1 downto 0) = "10") then
          if( TXDotCounter_x = "010" ) then
            TXDotCounter_x <= "000";
            TXCharCounter_x <= TXCharCounter_x + 1;
          else
            TXDotCounter_x <= TXDotCounter_x + 1;
          end if;
        end if;
      end if;

-- text1 window
      if( (h_counter( 1 downto 0) = "10") and ( dotCounter_x = "000000111" ) ) then
        twindow_x <= '1';
      elsif( (h_counter( 1 downto 0) = "10") and ( dotCounter_x = "011110111" ) ) then
        twindow_x <= '0';
      end if;

-- generate bwindow
      if( h_counter = 200-1 ) then
        bwindow_x <= '1';
      elsif( h_counter = CLOCKS_PER_LINE-1-1 ) then
        bwindow_x <= '0';
      end if;

      if( VdpR9InterlaceMode='0' ) then
        if( (v_counter = 10*2-1) or
            (v_counter = 524+10*2-1) ) then
          bwindow_y <= '1';
        elsif( (v_counter = 523-1) or
               (v_counter = 524+524-1) ) then
          bwindow_y <= '0';
        end if;
      else
        if( (v_counter = 10*2-1) or
            (v_counter = 525+10*2-1) ) then
          bwindow_y <= '1';
        elsif( (v_counter = 524-1) or
               (v_counter = 525+524-1) ) then
          bwindow_y <= '0';
        end if;
      end if;

      if( (bwindow_x = '1') and (bwindow_y = '1') )then
        bwindow <= '1';
      else
        bwindow <= '0';
      end if;

    end if;
  end process;


  -- color generator
  process( clk21m, reset )
    variable SpAttrTblBaseAddr : std_logic_vector(9 downto 0);
    variable SpPtnGeneTblBaseAddr : std_logic_vector(5 downto 0);
  begin
    if (reset = '1') then
      dotState <= (others => '0' );
      colorCode <= (others => '0' );
      iVideoR <= "000000";
      iVideoG <= "000000";
      iVideoB <= "000000";
      pVideoDHClk <= '0';
      pVideoDLClk <= '0';

      paletteWrAckRB <= '0';
      paletteWrAckG <= '0';
      paletteAddr_out <= (others => '0');
      paletteInRB <= '0';
      paletteInG <= '0';

      pRamAdr <= (others => '1');
      pRamDat <= (others => 'Z');
      pRamOe_n <= '1';
      pRamWe_n <= '1';

      SpPreReading <= '0';
      VdpVramReading <= '0';

      VdpVramRdAck <= '0';
      VdpVramWrAck <= '0';
      VdpVramRdData <= (others => '0');
      VdpVramAddrSetAck <= '0';
      VdpVramAccessAddr <= (others => '0');

    elsif (clk21m'event and clk21m = '1') then
      if( h_counter = CLOCKS_PER_LINE-1) then
        dotState <= "00";
        pVideoDHClk <= '1';
        pVideoDLClk <= '1';
      else
        case dotState is
          when "00" =>
            dotState <= "01";
            pVideoDHClk <= '0';
            pVideoDLClk <= '1';
          when "01" =>
            dotState <= "11";
            pVideoDHClk <= '1';
            pVideoDLClk <= '0';
          when "11" =>
            dotState <= "10";
            pVideoDHClk <= '0';
            pVideoDLClk <= '0';
          when "10" =>
            dotState <= "00";
            pVideoDHClk <= '1';
            pVideoDLClk <= '1';
          when others => null;
        end case;
      end if;

      --
      -- dotState     10 00 01 11 10 00 01 11 10
      --                |           |
      -- pRamCe_n    11\000000000000000000000
      -- pRamOe_n    11\00000/111111111111111
      -- pRamWe_n    11111111111111\00000/111
      -- pRamAdr       <     >-----<     >
      -- pRamDat       >ZZZZZZ
      --                       |
      --

     -- variable
--      if( v_counter = 200 ) then
      if( SpPreReadState = spstate_idle ) then
        if( SpMode2 = '0' ) then
          SpAttrTblBaseAddr := VdpR11R5SpAttrTblBaseAddr;
        else
--          SpAttrTblBaseAddr := VdpR11R5SpAttrTblBaseAddr(9 downto 3) & "100";
          SpAttrTblBaseAddr := VdpR11R5SpAttrTblBaseAddr(9 downto 2) & "00";
        end if;

        SpPtnGeneTblBaseAddr := VdpR6SpPtnGeneTblBaseAddr;
      end if;
--      end if;
      
-- main state
      case dotState is
        when "10" =>
          if( (VdpModeGraphic7 = '1') and (preWindow = '1') and
              (VramReadFreeFlag = '0' ) and
              (fifoFull = '0') and
              (VdpR1DispOn='1') ) then
            pRamAdr <= (preDotCounter_x(0) &
                        VdpR2PtnNameTblBaseAddr(5) &
                        preDotCounter_y(7 downto 0) & preDotCounter_x(7 downto 1));
            pRamDat <= (others => 'Z' );
            pRamOe_n <= '0';
            pRamWe_n <= '1';
          elsif( (VdpModeGraphic4 = '1') and (preWindow = '1') and
                 (VramReadFreeFlag = '0' ) and
                 (fifoFull = '0') and
                 (VdpR1DispOn='1') ) then
            pRamAdr <= (VdpR2PtnNameTblBaseAddr(6 downto 5) &
                        preDotCounter_y(7 downto 0) & preDotCounter_x( 6 downto 0));
            pRamDat <= (others => 'Z' );
            pRamOe_n <= '0';
            pRamWe_n <= '1';
          elsif( (VdpModeGraphic1 = '1') and (g123Window = '1') and
                 (dotcounter_x(0) = '1') and (VdpR1DispOn='1') ) then
            -- screen 1
            pRamDat <= (others => 'Z' );
            pRamOe_n <= '0';
            pRamWe_n <= '1';
            case dotCounter_x(2 downto 0) is
              when "011" =>
                -- read pattern name table
                pRamAdr <= (VdpR2PtnNameTblBaseAddr &
                            dotCounter_y(7 downto 3) & (dotCounter_x(7 downto 3)+1) );
              when "101" =>
                -- read pattern Generator table
                pRamAdr <= (VdpR4PtnGeneTblBaseAddr & G1PtnNum &
                            dotCounter_y(2 downto 0) );
              when "111" =>
                -- read color table
                pRamAdr <= (VdpR10R3ColorTblBaseAddr & '0' & G1PtnNum( 7 downto 3 ) );
              when others =>
                null;
            end case;
          elsif( (VdpModeGraphic2 = '1' or VdpModeGraphic3 = '1') and
                 (g123Window = '1') and (dotcounter_x(0) = '1') and (VdpR1DispOn='1') ) then
            -- screen 2, 4
            pRamDat <= (others => 'Z');
            pRamOe_n <= '0';
            pRamWe_n <= '1';
            case dotCounter_x(2 downto 0) is
              when "011" =>
                -- read pattern name table
                pRamAdr <= (VdpR2PtnNameTblBaseAddr &
                            dotCounter_y(7 downto 3) & (dotCounter_x(7 downto 3)+1) );
              when "101" =>
                -- read pattern Generator table
                pRamAdr <= (VdpR4PtnGeneTblBaseAddr(5 downto 2) &
                            dotCounter_y(7 downto 6) & G1PtnNum & dotCounter_y(2 downto 0) ) and
                           ("1111" & VdpR4PtnGeneTblBaseAddr(1 downto 0) & "11111111" & "111");
              when "111" =>
                -- read color table
                pRamAdr <= (VdpR10R3ColorTblBaseAddr(10 downto 7) &
                            dotCounter_y(7 downto 6) & G1PtnNum & dotCounter_y(2 downto 0) ) and
                           ("1111" & VdpR10R3ColorTblBaseAddr(6 downto 0) & "111111" );
              when others =>
                null;
            end case;
          elsif( (VdpModeText1 = '1') and (window = '1') and
                 (TXDotCounter_x(0) = '1') and (VdpR1DispOn='1') ) then
            -- text 1 (screen 0 width 40)
            pRamDat <= (others => 'Z' );
            pRamOe_n <= '0';
            pRamWe_n <= '1';
            case TXDotCounter_x is
              when "001" =>
                -- read pattern name table
                pRamAdr <= (VdpR2PtnNameTblBaseAddr & (T1CharCounter_y+TXCharCounter_x) );
              when "011" =>
                null;
              when "101" =>
                -- read pattern Generator table
                pRamAdr <= (VdpR4PtnGeneTblBaseAddr & T1PtnNum & dotCounter_y(2 downto 0) );
              when others =>
                null;
            end case;
          elsif( (VdpModeText2 = '1') and (window = '1') and
                 ((TXDotCounter_x = "001") or (TXDotCounter_x = "010")) and
                 (VdpR1DispOn='1') ) then
            -- text 2 (screen 0 width 80)
            pRamDat <= (others => 'Z' );
            pRamOe_n <= '0';
            pRamWe_n <= '1';
            case TXDotCounter_x is
              when "001" =>
                -- read pattern name table
                pRamAdr <= VdpR2PtnNameTblBaseAddr(6 downto 2) &
                           (T2CharCounter_y+TXCharCounter_x) ;
              when "010" =>
                -- read pattern Generator table
                pRamAdr <= (VdpR4PtnGeneTblBaseAddr & T2PtnNum & dotCounter_y(2 downto 0) );
              when others =>
                null;
            end case;
          elsif( VdpVramWrReq /= VdpVramWrAck ) then
            -- JP: VRAMへの書き込み
            -- JP: GRAPHIC6,7ではアドレスと RAM上の位置が他の画面モードと
            -- JP: 異るので注意
            if( VdpModeGraphic7 = '1' ) then
              pRamAdr <= VdpVramAccessAddr(0) &
                         VdpVramAccessAddr(16 downto 1);
            else
              pRamAdr <= VdpVramAccessAddr;
            end if;
            VdpVramAccessAddr <= VdpVramAccessAddr + 1;
            pRamDat <= VdpVramAccessData;
            pRamOe_n <= '1';
            pRamWe_n <= '0';
            VdpVramWrAck <= not VdpVramWrAck;
          elsif( VdpVramRdReq /= VdpVramRdAck ) then
            if( VdpVramAddrSetReq /= VdpVramAddrSetAck ) then
              pRamAdr <= VdpVramAccessAddrTmp;
              VdpVramAccessAddr <= VdpVramAccessAddrTmp + 1;
              VdpVramAddrSetAck <= not VdpVramAddrSetAck;
            else
              pRamAdr <= VdpVramAccessAddr;
              VdpVramAccessAddr <= VdpVramAccessAddr + 1;
            end if;
            pRamDat <= (others => 'Z');
            pRamOe_n <= '0';
            pRamWe_n <= '1';
            VdpVramRdAck <= not VdpVramRdAck;
            VdpVramReading <= '1';
          elsif( (SpPreReadState /= spstate_idle) and (VdpR8SpOff='0') and
                 ( VramReadFreeFlag = '0')  )then
            SpPreReading <= '1';
            pRamDat <= (others => 'Z' );
            pRamOe_n <= '0';
            pRamWe_n <= '1';
            
            case SpPreReadState is
              when spstate_yread =>
                pRamAdr <= SpAttrTblBaseAddr & SpPreReadCounter & "00";
              when spstate_xread =>
                pRamAdr <= SpAttrTblBaseAddr & SpPreReadCounter & "01";
              when spstate_ptnnumread =>
                pRamAdr <= SpAttrTblBaseAddr & SpPreReadCounter & "10";
              when spstate_colorread =>
                if( SpMode2 = '0' ) then
                  pRamAdr <= SpAttrTblBaseAddr & SpPreReadCounter & "11";
                else
                  -- sprite color table
--                  pRamAdr <= (SpAttrTblBaseAddr(9 downto 3) & "0" &
--                              SpPreReadCounter & SpPreReadY( 3 downto 0));
                  pRamAdr <= (SpAttrTblBaseAddr(9 downto 3) & not SpAttrTblBaseAddr(2) &
                              SpPreReadCounter & SpPreReadY( 3 downto 0));
                end if;
              when spstate_ptnread1 =>
                if( VdpR1SpSize = '0' ) then
                  -- 8x8 mode
                  pRamAdr <= (SpPtnGeneTblBaseAddr &
                              SpPreReadPtnNum( 7 downto 0) & SpPreReadY( 2 downto 0) );
                else
                  -- 16x16 mode
                  pRamAdr <= (SpPtnGeneTblBaseAddr &
                              SpPreReadPtnNum( 7 downto 2) & '0' & SpPreReadY( 3 downto 0) );
                end if;
              when spstate_ptnread2 =>
                if( VdpR1SpSize = '0' ) then
                  -- 8x8 mode
                  null;
                else
                  -- 16x16 mode
                  pRamAdr <= (SpPtnGeneTblBaseAddr &
                              SpPreReadPtnNum( 7 downto 2) & '1' & SpPreReadY( 3 downto 0) );
                end if;
              when others =>
                null;
            end case;
          end if;
          if( bwindow = '1' ) then
            if( VdpModeGraphic7 = '1' ) then
              iVideoR <= colorCode(4 downto 2) & "000";
              iVideoG <= colorCode(7 downto 5) & "000";
              iVideoB <= colorCode(1 downto 0) & colorCode(1) & "000";
            else
              iVideoR <= paletteDataRB_out(6 downto 4) & "000";
              iVideoB <= paletteDataRB_out(2 downto 0) & "000";
              iVideoG <= paletteDataG_out(2 downto 0) & "000";
            end if;
          else
            iVideoR <= (others => '0');
            iVideoG <= (others => '0');
            iVideoB <= (others => '0');
            iVideoB <= (others => '0');
          end if;

        when "00" =>
          pRamWe_n <= '1';
          pRamOe_n <= '1';

          if( VdpModeText2 = '1' ) then
            if( twindow = '0' ) then
              paletteAddr_out <= VdpR7FrameColor(3 downto 0);
            elsif( (VdpR8Color0On = '0') and (T2ColorCode = "0000") ) then 
              paletteAddr_out <= VdpR7FrameColor(3 downto 0);
            else
              paletteAddr_out <= T2ColorCode;
            end if;
          end if;
          
        when "01" =>
          if( paletteWrReqRB /= paletteWrAckRB ) then
            paletteInRB <= '1';
            paletteWrAckRB <= not paletteWrAckRB;
          elsif( paletteWrReqG /= paletteWrAckG ) then
            paletteInG <= '1';
            paletteWrAckG <= not paletteWrAckG;
          end if;
--          paletteInRB <= '0';
--          paletteInG <= '0';
          
          if( bwindow = '1' ) then
            if( VdpModeText2 = '1' ) then
              -- (Screen0 width 80)
              iVideoR <= paletteDataRB_out(6 downto 4) & "000";
              iVideoB <= paletteDataRB_out(2 downto 0) & "000";
              iVideoG <= paletteDataG_out(2 downto 0) & "000";
            end if;
          end if;

          if( VdpVramReading = '1' ) then
            VdpVramRdData <= pRamDat;
          end if;
          VdpVramReading <= '0';
--          pRamWe_n <= '1';
--          pRamOe_n <= '1';
          pRamDat <= (others => 'Z');
--                  pRamAdr <= (others => 'Z');

          if( (window = '1') and (VdpR1DispOn = '1') ) then
            -- JP: スプライトの表示
            if((SpPattern(0)(16) = '1') or (SpPattern(1)(16) = '1') or
               (SpPattern(2)(16) = '1') or (SpPattern(3)(16) = '1') or
               ((SpMode2 = '1') and
                ((SpPattern(4)(16) = '1') or (SpPattern(5)(16) = '1') or
                 (SpPattern(6)(16) = '1') or (SpPattern(7)(16) = '1'))) ) then
              colorCode <= "0000" & spColorCode_3 & spColorCode(2 downto 0);
            elsif( VdpModeGraphic1 = '1' ) then 
              if( (VdpR8Color0On = '0') and (G1ColorCode = "0000") ) then 
                colorCode <= VdpR7FrameColor;
              else
                colorCode <= "0000" & G1ColorCode;
              end if;
            elsif( VdpModeGraphic2 = '1' or VdpModeGraphic3 = '1' ) then 
              if( (VdpR8Color0On = '0') and (G1ColorCode = "0000") ) then 
                colorCode <= VdpR7FrameColor;
              else
                colorCode <= "0000" & G1ColorCode;
              end if;
            elsif( VdpModeGraphic4 = '1' ) then 
              if( (VdpR8Color0On = '0') and (G4ColorCode = "0000") ) then 
                colorCode <= VdpR7FrameColor;
              else
                colorCode <= "0000" & G4ColorCode;
              end if;
            elsif( VdpModeGraphic7 = '1' ) then 
              if( (VdpR8Color0On = '0') and (G7ColorCode = "00000000") ) then 
                colorCode <= VdpR7FrameColor;
              else
                colorCode <= G7ColorCode;
              end if;
            elsif( VdpModeText1 = '1' ) then
              if( twindow = '0' ) then
                colorCode <= VdpR7FrameColor;
              elsif( (VdpR8Color0On = '0') and (T1ColorCode = "0000") ) then 
                colorCode <= VdpR7FrameColor;
              else
                colorCode <= "0000" & T1ColorCode;
              end if;
            elsif( VdpModeText2 = '1' ) then
              if( twindow = '0' ) then
                colorCode <= VdpR7FrameColor;
              elsif( (VdpR8Color0On = '0') and (T2ColorCode = "0000") ) then 
                colorCode <= VdpR7FrameColor;
              else
                colorCode <= "0000" & T2ColorCode;
              end if;
            end if;
          else
            colorCode <= VdpR7FrameColor;
          end if;
        when "11" =>
          SpPreReading <= '0';
          paletteInRB <= '0';
          paletteInG <= '0';

          -- Palette decoding
          paletteAddr_out <= colorCode( 3 downto 0);

          if( VdpVramAddrSetReq /= VdpVramAddrSetAck ) then
            VdpVramAccessAddr <= VdpVramAccessAddrTmp;
            VdpVramAddrSetAck <= not VdpVramAddrSetAck;
          end if;

        when others =>
          null;
      end case;
    end if;
  end process;

  process( clk21m, reset )
  begin
    if (reset = '1') then
      T1ColorCode <= (others => '0');
      T1PtnNum <= (others => '0');
      T1Pattern <= (others => '0');
      T2ColorCode <= (others => '0');
      T2PtnNum <= (others => '0');
      T2Pattern <= (others => '0');
      G1ColorCode <= (others => '0');
      G1PtnNum <= (others => '0');
      G1Pattern <= (others => '0');
      G1PPattern <= (others => '0');
      G1Color <= (others => '0');
      G4ColorCode <= (others => '0');
      G7ColorCode <= (others => '0');
      fifoAddr_in <= (others => '0');
      fifoAddr_out <= (others => '0');
      fifoIn <= '0';
    elsif (clk21m'event and clk21m = '1') then
      -- text 1 state
      case dotState is
        when "10" =>
          null;
        when "00" =>
          if( T1Pattern(7) = '1' ) then
            T1ColorCode <= VdpR7FrameColor(7 downto 4);
          else
            T1ColorCode <= VdpR7FrameColor(3 downto 0);
          end if;
          T1Pattern <= T1Pattern(6 downto 0) & '0';
        when "01" =>
          case TXDotCounter_x is
            when "001" =>
              -- read pattern name table
              T1PtnNum <= pRamDat;
            when "011" =>
              null;
            when "101" =>
              -- read pattern Generator table
              T1Pattern <= pRamDat;
            when others =>
              null;
          end case;
        when "11" =>
          null;
        when others =>
          null;
      end case;

      -- text 2 state
      case dotState is
        when "00" | "11" =>
          if( T2Pattern(7) = '1' ) then
            T2ColorCode <= VdpR7FrameColor(7 downto 4);
          else
            T2ColorCode <= VdpR7FrameColor(3 downto 0);
          end if;
          T2Pattern <= T2Pattern(6 downto 0) & '0';
        when "01" =>
          case TXDotCounter_x is
            when "001" =>
              -- read pattern name table
              T2PtnNum <= pRamDat;
            when "010" =>
              -- read pattern Generator table
              T2Pattern <= pRamDat;
            when others =>
              null;
          end case;
        when "10" =>
          null;
        when others =>
          null;
      end case;
      
      -- graphic 1,2,3 state
      case dotState is
        when "10" =>
          null;
        when "00" =>
          if( G1Pattern(7) = '1' ) then
            G1ColorCode <= G1Color(7 downto 4);
          else
            G1ColorCode <= G1Color(3 downto 0);
          end if;
          G1Pattern <= G1Pattern(6 downto 0) & '0';
        when "01" =>
          case dotCounter_x(2 downto 0) is
            when "011" =>
              -- read pattern name table
              G1PtnNum <= pRamDat;
            when "101" =>
              -- read pattern Generator table
              G1PPattern <= pRamDat;
            when "111" =>
              -- read color table
              G1Color <= pRamDat;
              G1Pattern <= G1PPattern;
            when others =>
              null;
          end case;
        when "11" =>
          null;
        when others => null;
      end case;

      if( VdpModeGraphic4 = '1') then
        -- graphic 4 state
        case dotState is
          when "10" =>
            null;
          when "00" =>
            if( window = '1' ) then
              if( dotCounter_x(0) = '0' ) then
                G4colorCode <= fifoData_out(7 downto 4);
              else
                fifoAddr_out <= fifoAddr_out + 1;
                G4colorCode <= fifoData_out(3 downto 0);
              end if;
            else
              fifoAddr_out <= (others => '0');
            end if;
            if( (preWindow = '1') and (VramReadFreeFlag = '0' ) and
                (fifoFull = '0') ) then
              fifoIn <= '1';
            end if;
          when "01" =>
            if( preWindow = '0' ) then
              fifoAddr_in <= (others => '0');
            elsif( fifoIn = '1' ) then
              fifoIn <= '0';
              fifoAddr_in <= fifoAddr_in + 1;
            end if;
          when "11" =>
            null;
          when others => null;
        end case;
      elsif( VdpModeGraphic7 = '1' ) then
        -- graphic 7 state
        case dotState is
          when "10" =>
            null;
          when "00" =>
            if( window = '1' ) then
              fifoAddr_out <= fifoAddr_out + 1;
              G7colorCode <= fifoData_out;
            else
              fifoAddr_out <= (others => '0');
            end if;
            if( (preWindow = '1') and (VramReadFreeFlag = '0' ) and
                (fifoFull = '0') ) then
              fifoIn <= '1';
            end if;
          when "01" =>
            if( preWindow = '0' ) then
              fifoAddr_in <= (others => '0');
            elsif( fifoIn = '1' ) then
              fifoIn <= '0';
              fifoAddr_in <= fifoAddr_in + 1;
            end if;
          when "11" =>
            null;
          when others => null;
        end case;
      end if;
    end if;
  end process;
  
  -- sprite generator 
  spColorCode_3 <=
    ((SpColorIn(0) and SpPattern(0)(16)) or
     ( not SpPattern(0)(16) and
       (SpColorIn(1) and SpPattern(1)(16)) ) or
     ( not SpPattern(0)(16) and not SpPattern(1)(16) and
       (SpColorIn(2) and SpPattern(2)(16)) ) or
     ( not SpPattern(0)(16) and not SpPattern(1)(16) and not SpPattern(2)(16) and
       (SpColorIn(3) and SpPattern(3)(16)) ))
    when (SpMode2 = '0') else 
    (((SpColorIn(0) and SpPattern(0)(16) ) or
      (SpColorIn(1) and SpPattern(1)(16) and SpCC(1) ) or
      (SpColorIn(2) and SpPattern(2)(16) and SpCC(1) and SpCC(2) ) or
      (SpColorIn(3) and SpPattern(3)(16) and SpCC(1) and SpCC(2) and SpCC(3) ) or
      (SpColorIn(4) and SpPattern(4)(16) and SpCC(1) and SpCC(2) and SpCC(3) and SpCC(4) ) or
      (SpColorIn(5) and SpPattern(5)(16) and SpCC(1) and SpCC(2) and SpCC(3) and SpCC(4) and SpCC(5) ) or
      (SpColorIn(6) and SpPattern(6)(16) and SpCC(1) and SpCC(2) and SpCC(3) and SpCC(4) and SpCC(5) and SpCC(6) ) or
      (SpColorIn(7) and SpPattern(7)(16) and SpCC(1) and SpCC(2) and SpCC(3) and SpCC(4) and SpCC(5) and SpCC(6) and SpCC(7) ))
     or
     (( not SpPattern(0)(16) and not SpCC(1) ) and
      ((SpColorIn(1) and SpPattern(1)(16) ) or
       (SpColorIn(2) and SpPattern(2)(16) and SpCC(2) ) or
       (SpColorIn(3) and SpPattern(3)(16) and SpCC(2) and SpCC(3) ) or
       (SpColorIn(4) and SpPattern(4)(16) and SpCC(2) and SpCC(3) and SpCC(4) ) or
       (SpColorIn(5) and SpPattern(5)(16) and SpCC(2) and SpCC(3) and SpCC(4) and SpCC(5) ) or
       (SpColorIn(6) and SpPattern(6)(16) and SpCC(2) and SpCC(3) and SpCC(4) and SpCC(5) and SpCC(6) ) or
       (SpColorIn(7) and SpPattern(7)(16) and SpCC(2) and SpCC(3) and SpCC(4) and SpCC(5) and SpCC(6) and SpCC(7) )))
     or
     (( not SpPattern(0)(16) and not SpPattern(1)(16) and not SpCC(2) ) and
      ((SpColorIn(2) and SpPattern(2)(16) ) or
       (SpColorIn(3) and SpPattern(3)(16) and SpCC(3) ) or
       (SpColorIn(4) and SpPattern(4)(16) and SpCC(3) and SpCC(4) ) or
       (SpColorIn(5) and SpPattern(5)(16) and SpCC(3) and SpCC(4) and SpCC(5) ) or
       (SpColorIn(6) and SpPattern(6)(16) and SpCC(3) and SpCC(4) and SpCC(5) and SpCC(6) ) or
       (SpColorIn(7) and SpPattern(7)(16) and SpCC(3) and SpCC(4) and SpCC(5) and SpCC(6) and SpCC(7) )))
     or
     (( not SpPattern(0)(16) and not SpPattern(1)(16) and not SpPattern(2)(16) and not SpCC(3) ) and
      ((SpColorIn(3) and SpPattern(3)(16) ) or 
       (SpColorIn(4) and SpPattern(4)(16) and SpCC(4) ) or
       (SpColorIn(5) and SpPattern(5)(16) and SpCC(4) and SpCC(5) ) or
       (SpColorIn(6) and SpPattern(6)(16) and SpCC(4) and SpCC(5) and SpCC(6) ) or
       (SpColorIn(7) and SpPattern(7)(16) and SpCC(4) and SpCC(5) and SpCC(6) and SpCC(7) )))
     or
     (( not SpPattern(0)(16) and not SpPattern(1)(16) and not SpPattern(2)(16) and
        not SpPattern(3)(16) and not SpCC(4) ) and
      ((SpColorIn(4) and SpPattern(4)(16) ) or
       (SpColorIn(5) and SpPattern(5)(16) and SpCC(5) ) or
       (SpColorIn(6) and SpPattern(6)(16) and SpCC(5) and SpCC(6) ) or
       (SpColorIn(7) and SpPattern(7)(16) and SpCC(5) and SpCC(6) and SpCC(7) )))
     or
     (( not SpPattern(0)(16) and not SpPattern(1)(16) and not SpPattern(2)(16) and
        not SpPattern(3)(16) and not SpPattern(4)(16) and not SpCC(5) ) and
      ((SpColorIn(5) and SpPattern(5)(16) ) or
       (SpColorIn(6) and SpPattern(6)(16) and SpCC(6) ) or
       (SpColorIn(7) and SpPattern(7)(16) and SpCC(6) and SpCC(7) )))          
     or
     (( not SpPattern(0)(16) and not SpPattern(1)(16) and not SpPattern(2)(16) and
        not SpPattern(3)(16) and not SpPattern(4)(16) and not SpPattern(5)(16) and
        not SpCC(6) ) and
      ((SpColorIn(6) and SpPattern(6)(16) ) or
       (SpColorIn(7) and SpPattern(7)(16) and SpCC(7) )))
     or
     (( not SpPattern(0)(16) and not SpPattern(1)(16) and not SpPattern(2)(16) and
        not SpPattern(3)(16) and not SpPattern(4)(16) and not SpPattern(5)(16) and
        not SpPattern(6)(16) and not SpCC(7) ) and
      ((SpColorIn(7) and SpPattern(7)(16) ))) );
  
  process( clk21m, reset )
  begin
    if (reset = '1') then
      for i in 0 to SpMode2_nSprites -1 loop
        SpPattern(i) <= (others => '0');
        SpColor(i) <= (others => '0');
        SpColorIn(i) <= '0';
        SpX(i) <= (others => '0');
        SpIC(i) <= '0';
        SpEC(i) <= '0';
        SpCC(i) <= '0';
      end loop;
      SpReadIsContinuous <= '0';
      spColorIsTransparent <= '0';
      spColorCode <= (others => '0' );
      SpPreReadPtnNum <= (others => '0' );
      SpPreReadY <= (others => '0' );
      SpPreReadState <= spstate_idle;
      SpPreReadCounter <= (others => '0' );
      SpPreReadCounter2 <= (others => '0' );
      SpY <= (others => '0');
    elsif (clk21m'event and clk21m = '1') then
      case dotState is
        when "10" =>
          for i in 0 to SpMode2_nSprites-1 loop
            SpColorIn(i) <= SpColor(i)(2);
          end loop;
          spColorCode(1) <= spColorCode_3;
          if( (preDotCounter_x_end = '1') and (spwindow_y = '1') ) then
            SpPreReadState <= spstate_idle;
            SpPreReadCounter <= (others => '0');
            SpPreReadCounter2 <= (others => '0');
            SpY <= dotCounter_y;
          end if;
        when "00" =>
          for i in 0 to SpMode2_nSprites-1 loop
            SpColorIn(i) <= SpColor(i)(3);
          end loop;
          spColorCode(2) <= spColorCode_3;
        when "01" =>
          for i in 0 to SpMode2_nSprites-1 loop
            SpColorIn(i) <= SpColor(i)(0);
          end loop;
          if( SpPreReading = '1' ) then
-- JP: 水平帰線期間にそのラインで表示するスプライトの情報を集める
-- JP: スプライトアトリビュートテーブルの構造
-- Sprite Attribute Table
--        7 6 5 4 3 2 1 0 
--  +0 : |       Y       |
--  +1 : |       X       |
--  +2 : | Pattern Num   |
--  +3 : |EC0 0 0| Color |
--
            case SpPreReadState is
              when spstate_yread =>
                SpPreReadY <= SpY - pRamDat;
                if( (SpMode2 = '0' and pRamDat = "11010000" ) or       -- Y = 208
                    (SpMode2 = '1' and pRamDat = "11011000" ) ) then   -- Y = 216
                  -- JP: Y座標が 208(SpMode1) or 216(SpMode2)の時，
                  -- JP: 以降のスプライトを OFF
                  SpDispEnd <= '1';
                end if;
              when spstate_xread =>
                SpX(conv_integer(SpPreReadCounter2)) <= '0' & pRamDat;
              when spstate_ptnnumread =>
                SpPreReadPtnNum <= pRamDat;
              when spstate_colorread =>
                SpColor(conv_integer(SpPreReadCounter2)) <= pRamDat( 3 downto 0);
                SpEC(conv_integer(SpPreReadCounter2)) <= pRamDat(7);
                SpCC(conv_integer(SpPreReadCounter2)) <= pRamDat(6);
                SpIC(conv_integer(SpPreReadCounter2)) <= pRamDat(5);
                if( (pRamDat(3 downto 0) = "0000") or
                    (pRamDat(6) = '1' and SpReadIsContinuous = '0') ) then
                  -- JP: 透明色は表示されない.
                  -- JP: CCが 1の時，直前のスプライトプレーンが同一ラインに無い時は
                  -- JP: 表示されない.
                  spColorIsTransparent <= '1';
                else
                  spColorIsTransparent <= '0';
                end if;
              when spstate_ptnread1 =>
                if( spColorIsTransparent = '1' ) then
                  SpPattern(conv_integer(SpPreReadCounter2))(16 downto 8) <= (others => '0');
                else
                  SpPattern(conv_integer(SpPreReadCounter2))(16 downto 8) <= '0' & pRamDat;
                end if;
              when spstate_ptnread2 =>
                if( spColorIsTransparent = '1' ) then
                  SpPattern(conv_integer(SpPreReadCounter2))(7 downto 0) <= (others => '0');
                elsif( VdpR1SpSize = '0' ) then
                  -- 8x8 mode
                  SpPattern(conv_integer(SpPreReadCounter2))(7 downto 0) <= (others => '0');
                else
                  -- 16x16 mode
                  SpPattern(conv_integer(SpPreReadCounter2))(7 downto 0) <= pRamDat;
                end if;
              when others =>
                null;
            end case;
          else
            for i in 0 to SpMode2_nSprites - 1 loop
              if( (SpX(i)(8) = '1') and
                  ( (VdpR1SpZoom='0') or ((VdpR1SpZoom='1')and(SpX(i)(0)='0')) ) ) then
                SpPattern(i) <= SpPattern(i)(15 downto 0) & '0';
              end if;
              if( spwindow = '1' ) then
                SpX(i) <= SpX(i) - 1;
              elsif( spwindow_ec = '1' ) then
                if( SpEC(i)='1' ) then
                  SpX(i) <= SpX(i) - 1;
                end if; 
              end if;
            end loop;
          end if;                       
        when "11" =>
          for i in 0 to SpMode2_nSprites - 1 loop
            SpColorIn(i) <= SpColor(i)(1);
          end loop;
          spColorCode(0) <= spColorCode_3;
          
          if( SpDispEnd = '1' ) then
            -- JP: Y座標が 208の時，以降のスプライトを OFF
            SpPreReadState <= spstate_idle;
            SpDispEnd <= '0';
          elsif( (SpPreReadState = spstate_idle) and (SpPreReadCounter = "00000") ) then
            SpPreReadState <= spstate_yread;
            SpReadIsContinuous <= '0';
            for i in 0 to SpMode2_nSprites - 1 loop
              SpPattern(i) <= (others =>'0');
            end loop;
          elsif( SpPreReading = '1' ) then
            case SpPreReadState is
              when spstate_yread =>
                if( (SpPreReadY( 7 downto 3) = "00000") and (VdpR1SpSize = '0' ) and (VdpR1SpZoom='0') )then
                  SpPreReadState <= spstate_xread;
                elsif( (SpPreReadY( 7 downto 4) = "0000") and (VdpR1SpSize = '1' ) and (VdpR1SpZoom='0') )then
                  SpPreReadState <= spstate_xread;
                elsif( (SpPreReadY( 7 downto 4) = "0000") and (VdpR1SpSize = '0' ) and (VdpR1SpZoom='1') )then
                  SpPreReadState <= spstate_xread;
                  SpPreReadY <= '0' & SpPreReadY(7 downto 1);
                elsif( (SpPreReadY( 7 downto 5) = "000") and (VdpR1SpSize = '1' ) and (VdpR1SpZoom='1') )then
                  SpPreReadState <= spstate_xread;
                  SpPreReadY <= '0' & SpPreReadY(7 downto 1);
                elsif( SpPreReadCounter = "11111" ) then
                  SpPreReadState <= spstate_idle;
                else
                  SpPreReadState <= spstate_yread;
                  SpPreReadCounter <= SpPreReadCounter + 1;
                  -- JP: 連続したスプライトプレーンが表示されない時、SpReadIsContinuous をクリア
                  SpReadIsContinuous <= '0';
                end if;
              when spstate_xread =>
                SpPreReadState <= spstate_ptnnumread;
              when spstate_ptnnumread =>
                SpPreReadState <= spstate_colorread;
              when spstate_colorread =>
                SpPreReadState <= spstate_ptnread1;
              when spstate_ptnread1 =>
                SpPreReadState <= spstate_ptnread2;
              when spstate_ptnread2 =>
                if( (SpMode2='0') and (SpPreReadCounter2 = SpMode1_nSprites-1) ) then
                  -- JP: 横４つ分のスプライトを読んだ
                  SpPreReadState <= spstate_idle;
                elsif( (SpMode2='1') and (SpPreReadCounter2 = SpMode2_nSprites-1) ) then
                  -- JP: 横８つ分のスプライトを読んだ
                  SpPreReadState <= spstate_idle;
                elsif( SpPreReadCounter = "11111" ) then
                  -- JP: 32枚のスプライトプレーンの調査終了
                  SpPreReadState <= spstate_idle;
                else
                  SpPreReadCounter <= SpPreReadCounter + 1;
                  SpPreReadCounter2 <= SpPreReadCounter2 + 1;
                  -- JP: SpReadIsContinueをセット
                  SpReadIsContinuous <= '1';
                  SpPreReadState <= spstate_yread;
                end if;
              when others =>
                null;
            end case;
          end if;                       
        when others =>
          null;
      end case;
      
    end if;
  end process;

  
  -- VDP register access 
  process( clk21m, reset )
  begin
    if (reset = '1') then
      dbi <= (others => '0');
      vsyncIntAck <= '0';
--            dVsyncIntReq <= '0';
      VdpP1Data <= (others => '0');
      VdpP1Is1stByte <= '1';
      VdpP2Is1stByte <= '1';
      VdpRegWrPulse <= '0';
      VdpRegPtr <= (others => '0');
      VdpVramWrReq <= '0';
      VdpVramRdReq <= '0';
      VdpVramAddrSetReq <= '0';
      VdpVramAccessRw <= '0';
      VdpVramAccessAddrTmp <= (others => '0');
      VdpVramAccessData <= (others => '0');
      VdpR0DispNum <= (others => '0');
      VdpR0HSyncIntEn <= '0';
      VdpR1DispMode <= (others => '0');
      VdpR1SpSize <= '0';
      VdpR1SpZoom <= '0';
      VdpR1VSyncIntEn <= '0';
      VdpR1DispOn <= '0';
      VdpR2PtnNameTblBaseAddr <= (others => '0');
      VdpR4PtnGeneTblBaseAddr <= (others => '0');
      VdpR10R3ColorTblBaseAddr <= (others => '0');
      VdpR11R5SpAttrTblBaseAddr <= (others => '0');
      VdpR6SpPtnGeneTblBaseAddr <= (others => '0');
      VdpR7FrameColor <= (others => '0');
      VdpR8SpOff <= '0';
      VdpR8Color0On <= '0';
      VdpR9TwoPageMode <= '0';
      VdpR9InterlaceMode <= '0';
      VdpR9YDots <= '0';
      VdpR15StatusRegNum <= (others => '0');
      VdpR16PalNum <= (others => '0');
      VdpR17RegNum <= (others => '0');
      VdpR17IncRegNum <= '0';
      VdpR18Adjust <= (others => '0');
      VdpR19HSyncIntLine <= (others => '0');
      VdpR23VStartLine <= (others => '0');
      -- palette
      paletteWrTemp <= (others => '0');
      paletteWrReqRB <= '0';
      paletteWrReqG <= '0';
      paletteWrNum <= (others => '0');

    elsif (clk21m'event and clk21m = '1') then

      if (req = '1' and wrt = '0') then
        case adr(1 downto 0) is
          when "00"   => -- port#0 read
            dbi <= VdpVramRdData;
            VdpVramRdReq <= not VdpVramRdAck;
          when "01"   => -- port#1 read
            -- read status registers

            -- JP: ステータスレジスタを読む時に 1stバイトフラグをリセットしてみる.
            -- JP: 実機の挙動と合うかどうか不明.
            VdpP1Is1stByte <= '1';
            VdpP2Is1stByte <= '1';

            case VdpR15StatusRegNum is
              when "0000" =>
                -- 未実装あり
                if( vsyncIntAck /= vsyncIntReq ) then
                  vsyncIntAck <= not vsyncIntAck;
                  dbi <= '1' & "0000000";
                else
                  dbi <= '0' & "0000000";
                end if;
              when "0001" =>
                -- 未実装あり
                if( hsyncIntAck /= hsyncIntReq ) then
                  hsyncIntAck <= not hsyncIntAck;
                  dbi <= "00" & VDP_ID & '1';
                else
                  dbi <= "00" & VDP_ID & '0';
                end if;
              when "0010" =>
--                dbi <= '1' & not iVideoVS_n & not iVideoHS_n & '0' & "11" & field & '0';
                dbi <= '1' & not bwindow_y & not bwindow_x & '0' & "11" & field & '0';
              when others =>
                dbi <= (others => '0');
            end case;
          when "10"   => -- port#2 read
            dbi <= (others => '1');
          when others => -- port#3 read
            dbi <= (others => '1');
        end case;

      elsif (req = '1' and wrt = '1') then
        case adr(1 downto 0) is
          when "00"   => -- port#0 write
            VdpVramAccessData <= dbo;
            VdpVramWrReq <= not VdpVramWrAck;
          when "01"   => -- port#1 write
            if(VdpP1Is1stByte = '1') then
              VdpP1Is1stByte <= '0';
              VdpP1Data <= dbo;
            else
              VdpP1Is1stByte <= '1';
              case dbo( 7 downto 6 ) is
                when "01" =>  -- set vram access address(write)
                  VdpVramAccessAddrTmp( 7 downto 0 ) <= VdpP1Data( 7 downto 0);
                  VdpVramAccessAddrTmp(13 downto 8 ) <= dbo( 5 downto 0);
                  VdpVramAddrSetReq <= not VdpVramAddrSetAck;
                  VdpVramAccessRw <= '0';
                when "00" =>  -- set vram access address(read)
                  VdpVramAccessAddrTmp( 7 downto 0 ) <= VdpP1Data( 7 downto 0);
                  VdpVramAccessAddrTmp(13 downto 8 ) <= dbo( 5 downto 0);
                  VdpVramAddrSetReq <= not VdpVramAddrSetAck;
                  VdpVramAccessRw <= '1';
                  VdpVramRdReq <= not VdpVramRdAck;
                when "10" =>  -- JP: 直接レジスタ指定アクセス
                  VdpRegPtr <= dbo( 5 downto 0);
                  VdpRegWrPulse <= '1';
                when "11" =>  -- JP: 直接レジスタ指定アクセス?
                  VdpRegPtr <= dbo( 5 downto 0);
                  VdpRegWrPulse <= '1';
                when others =>
                  null;
              end case;
            end if;

          when "10"   => -- port#2 write
            if(VdpP2Is1stByte = '1') then
              paletteWrTemp <= dbo;
              paletteWrNum <= VdpR16PalNum;
              paletteWrReqRB <= not paletteWrAckRB;
              VdpP2Is1stByte <= '0';
            else
              paletteWrTemp <= dbo;
              paletteWrNum <= VdpR16PalNum;
              paletteWrReqG <= not paletteWrAckG;
              VdpP2Is1stByte <= '1';
              VdpR16PalNum <= VdpR16PalNum + 1;
            end if;

          when "11" => -- port#3 write
            -- JP: 間接指定では R#17の値は書き換えられない
            if( VdpR17RegNum /= "010001" ) then
              VdpRegWrPulse <= '1';
            end if;
            VdpP1Data <= dbo;
            VdpRegPtr <= VdpR17RegNum;
            if( VdpR17IncRegNum = '1' ) then
              VdpR17RegNum <= VdpR17RegNum + 1;
            end if;

          when others =>
            null;
        end case;

      elsif (VdpRegWrPulse = '1') then -- register write
        VdpRegWrPulse <= '0';
        case VdpRegPtr is
          when "000000" =>   -- #00
            VdpR0DispNum <= VdpP1Data(3 downto 1);
            VdpR0HSyncIntEn <= VdpP1Data(4);
          when "000001" =>   -- #01
            VdpR1SpZoom <= VdpP1Data(0);
            VdpR1SpSize <= VdpP1Data(1);
            VdpR1DispMode <= VdpP1Data(4 downto 3);
            VdpR1VSyncIntEn <= VdpP1Data(5);
            VdpR1DispOn <= VdpP1Data(6);
          when "000010" =>   -- #02
            VdpR2PtnNameTblBaseAddr <= VdpP1Data( 6 downto 0);
          when "000011" =>   -- #03
            VdpR10R3ColorTblBaseAddr(7 downto 0) <= VdpP1Data( 7 downto 0);
          when "000100" =>   -- #04
            VdpR4PtnGeneTblBaseAddr <= VdpP1Data( 5 downto 0);
          when "000101" =>   -- #05
            VdpR11R5SpAttrTblBaseAddr(7 downto 0) <= VdpP1Data;
          when "000110" =>   -- #06
            VdpR6SpPtnGeneTblBaseAddr <= VdpP1Data( 5 downto 0);
          when "000111" =>   -- #07
            VdpR7FrameColor <= VdpP1Data( 7 downto 0 );
          when "001000" =>   -- #08
            VdpR8SpOff <= VdpP1Data(1);
            VdpR8Color0On <= VdpP1Data(5);
          when "001001" =>   -- #09
            VdpR9TwoPageMode <= VdpP1Data(2);
            VdpR9InterlaceMode <= VdpP1Data(3);
            VdpR9YDots <= VdpP1Data(7);
          when "001010" =>   -- #10
            VdpR10R3ColorTblBaseAddr(10 downto 8) <= VdpP1Data( 2 downto 0);
          when "001011" =>   -- #11
            VdpR11R5SpAttrTblBaseAddr( 9 downto 8) <= VdpP1Data( 1 downto 0);
          when "001110" =>   -- #14
            VdpVramAccessAddrTmp( 16 downto 14 ) <= VdpP1Data( 2 downto 0);
            VdpVramAddrSetReq <= not VdpVramAddrSetAck;
          when "001111" =>   -- #15
            VdpR15StatusRegNum <= VdpP1Data( 3 downto 0);
          when "010000" =>   -- #16
            VdpR16PalNum <= VdpP1Data( 3 downto 0 );
          when "010001" =>   -- #17
            VdpR17RegNum <= VdpP1Data( 5 downto 0 );
            VdpR17IncRegNum <= not VdpP1Data(7);
          when "010010" =>   -- #18
            VdpR18Adjust <= VdpP1Data;
          when "010011" =>   -- #19
            VdpR19HSyncIntLine <= VdpP1Data;
          when "010111" =>    -- #23
            VdpR23VStartLine <= VdpP1Data;
          when others => null;
        end case;

      end if;

    end if;
  end process;

  -- Display resolution (0=15kHz, 1=31kHz)
  dispModeVGA <= DispReso;

end rtl;
