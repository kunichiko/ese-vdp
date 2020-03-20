----------------------------------------------------------------
--  Title     : msxation.vhd
--  Function  : PSG + SCC + RAM(EseSCC/SNATCHER compatible)
--  Date      : 4th,January,2003
--  Revision  : 3.00
--  Author    : Kazuhiro TSUJIKAWA (ESE Artists' factory)
----------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

----------------------------------------------------------------
-- CAUTION !!!!!!
--
-- " --@ " = temporary description (for debugging)
--
----------------------------------------------------------------

-- slave mode
-- Slot x-0 : none
-- Slot x-1 : ESE-RAM         00000-1FFFF / 20000-3FFFF(BIOS)
-- Slot x-2 : Mapper          40000-5FFFF / 60000-7FFFF(VRAM)
-- Slot x-3 : none

-- master mode
-- Slot 0-0 : MAINROM         20000-27FFF
-- Slot 0-2 : (SUBROM)+FMBIOS 28000-2FFFF
-- Slot 1   : CONNECTOR
-- Slot 2   : ESE-RAM         00000-1FFFF / 20000-3FFFF(BIOS)
-- Slot 3-0 : Mapper          40000-5FFFF / 60000-7FFFF(VRAM)
-- Slot 3-1 : SUBROM+KANJI    30000-3FFFF

-- 256kB : 00000-3FFFFh / ESE-SCC
--  64kB : 40000-4FFFFh / DISK+SUB+MAIN
--  64kB : 50000-5FFFFh / Mapper
-- 128kB : 60000-7FFFFh / VRAM

entity kuni_top is
  port(
    -- VDP clock ... 21.48MHz
    pClk21m     : in std_logic;

    -- MSX slot signals
    pSltClk     : in std_logic;
    pSltRst_n   : in std_logic;
    pSltSltsl_n : inout std_logic;
    pSltIorq_n  : inout std_logic;
    pSltRd_n    : inout std_logic;
    pSltWr_n    : inout std_logic;
    pSltAdr     : inout std_logic_vector(15 downto 0);
    pSltDat     : inout std_logic_vector(7 downto 0);
    pSltBdir_n  : out std_logic;

--  pSltCs1     : inout std_logic;
--  pSltCs2     : inout std_logic;
    pSltCs12    : inout std_logic;
    pSltRfsh_n  : inout std_logic;
    pSltWait_n  : inout std_logic;
    pSltInt_n   : inout std_logic;
    pSltM1_n    : inout std_logic;
    pSltMerq_n  : inout std_logic;

--  pSltClk2    : in std_logic;
--  pSltRsv5    : out std_logic;
--  pSltRsv16   : out std_logic;

    -- SRAM signals
    pRamCeX     : out std_logic;
    pRamOeX_n   : out std_logic;
    pRamWeX_n   : out std_logic;
    pRamAdrX    : out std_logic_vector(18 downto 0);
    pRamDatX    : inout std_logic_vector(7 downto 0);

--  pRamCeY_n   : out std_logic;
--  pRamOeY_n   : out std_logic;
--  pRamWeY_n   : out std_logic;
--  pRamAdrY    : out std_logic_vector(18 downto 0);
--  pRamDatY    : inout std_logic_vector(7 downto 0);

    -- Sound output
    pSltSnd_L   : inout std_logic_vector( 5 downto 0);    -- KuniBoard only
--    pSltSnd_R   : inout std_logic_vector( 5 downto 0);    -- KuniBoard only
    pSltSnd_R   : inout std_logic_vector( 4 downto 1);
    pPs2Clk     : inout std_logic;
    pPs2Dat     : inout std_logic;
--  pSltSound   : inout std_logic_vector( 5 downto 0);    -- KuniBoard only

    pSelf       : in std_logic;
    pClkEna_n   : out std_logic;                          -- KuniBoard only
    pCpuClk     : out std_logic;                          -- KuniBoard only
    pBusReq_n   : inout std_logic;
    pCpuM1_n    : inout std_logic;                        -- KuniBoard only
    pCpuRfsh_n  : inout std_logic;                        -- KuniBoard only

    pKeyCaps    : out std_logic;    -- Kana lamp : 0=ON, Z=OFF
    pKeyKana    : out std_logic;    -- Kana lamp : 0=ON, Z=OFF
    pKeyY       : inout std_logic_vector( 9 downto 0);
    pKeyX       : inout std_logic_vector( 7 downto 0);
    pJoyA       : inout std_logic_vector( 5 downto 0);
    pStrA       : out std_logic;
    pJoyB       : inout std_logic_vector( 5 downto 0);
    pStrB       : out std_logic;

--  pVideoClk   : out std_logic;    -- Video DAC(MB40988) clock
--  pVideoDHClk : out std_logic;    -- dot clock high resolution (10.74MHz)
--  pVideoDLClk : out std_logic;    -- dot clock low  resolution ( 5.37MHz)

    pVideoR     : out std_logic_vector( 5 downto 0);      -- KuniBoard only
    pVideoG     : out std_logic_vector( 5 downto 0);      -- KuniBoard only
    pVideoB     : out std_logic_vector( 5 downto 0);      -- KuniBoard only
    pVideoHS_n  : out std_logic;                          -- KuniBoard only
    pVideoVS_n  : out std_logic;                          -- KuniBoard only
--    pVideoCS_n  : out std_logic;                          -- KuniBoard only
    pVideoSC    : out std_logic;                          -- KuniBoard only
    pVideoSYNC  : out std_logic                           -- KuniBoard only
 );
end kuni_top;

architecture rtl of kuni_top is

  component t80a
    port(
      RESET_n : in std_logic;
      CLK_n   : in std_logic;
      WAIT_n  : in std_logic;
      INT_n   : in std_logic;
      NMI_n   : in std_logic;
      BUSRQ_n : in std_logic;
      M1_n    : out std_logic;
      MREQ_n  : out std_logic;
      IORQ_n  : out std_logic;
      RD_n    : out std_logic;
      WR_n    : out std_logic;
      RFSH_n  : out std_logic;
      HALT_n  : out std_logic;
      BUSAK_n : out std_logic;
      A       : out std_logic_vector(15 downto 0);
      D       : inout std_logic_vector(7 downto 0)
    );
  end component;

  component psg
    port(
      clk21m  : in std_logic;
      reset   : in std_logic;
      clkena  : in std_logic;
      req     : in std_logic;
      ack     : out std_logic;
      wrt     : in std_logic;
      adr     : in std_logic_vector(15 downto 0);
      dbi     : out std_logic_vector(7 downto 0);
      dbo     : in std_logic_vector(7 downto 0);

      joya    : inout std_logic_vector(5 downto 0);
      stra    : out std_logic;
      joyb    : inout std_logic_vector(5 downto 0);
      strb    : out std_logic;

      kana    : out std_logic;
      cmtin   : in std_logic;
      keymode : in std_logic;

      wave    : out std_logic_vector(7 downto 0)
    );
  end component;

  component rom
    port(
      clk     : in std_logic;
      adr     : in std_logic_vector(7 downto 0);
      dbi     : out std_logic_vector(7 downto 0)
    );
  end component;

  component esescc
    port(
      clk21m  : in std_logic;
      reset   : in std_logic;
      clkena  : in std_logic;
      req     : in std_logic;
      ack     : out std_logic;
      wrt     : in std_logic;
      adr     : in std_logic_vector(15 downto 0);
      dbi     : out std_logic_vector(7 downto 0);
      dbo     : in std_logic_vector(7 downto 0);

      ramreq  : out std_logic;
      ramwrt  : out std_logic;
      ramadr  : out std_logic_vector(19 downto 0);
      ramdbi  : in std_logic_vector(7 downto 0);
      ramdbo  : out std_logic_vector(7 downto 0);

      wavl    : out std_logic_vector(7 downto 0);
      wavr    : out std_logic_vector(7 downto 0)
    );
  end component;

  component eseram
    port(
      clk21m  : in std_logic;
      reset   : in std_logic;
      clkena  : in std_logic;
      req     : in std_logic;
      ack     : out std_logic;
      wrt     : in std_logic;
      adr     : in std_logic_vector(15 downto 0);
      dbi     : out std_logic_vector(7 downto 0);
      dbo     : in std_logic_vector(7 downto 0);

      ramreq  : out std_logic;
      ramwrt  : out std_logic;
      ramadr  : out std_logic_vector(19 downto 0);
      ramdbi  : in std_logic_vector(7 downto 0);
      ramdbo  : out std_logic_vector(7 downto 0)
    );
  end component;

  component mapper
    port(
      clk21m  : in std_logic;
      reset   : in std_logic;
      clkena  : in std_logic;
      req     : in std_logic;
      ack     : out std_logic;
      mem     : in std_logic;
      wrt     : in std_logic;
      adr     : in std_logic_vector(15 downto 0);
      dbi     : out std_logic_vector(7 downto 0);
      dbo     : in std_logic_vector(7 downto 0);

      ramreq  : out std_logic;
      ramwrt  : out std_logic;
      ramadr  : out std_logic_vector(21 downto 0);
      ramdbi  : in std_logic_vector(7 downto 0);
      ramdbo  : out std_logic_vector(7 downto 0)
    );
  end component;

  component vdp
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
    pVideoSYNC : out std_logic

    -- Display resolution (0=15kHz, 1=31kHz)
    DispReso : in  std_logic
    );
  end component;

  component rtc
    port(
      clk21m  : in std_logic;
      reset   : in std_logic;
      clkena  : in std_logic;
      req     : in std_logic;
      ack     : out std_logic;
      wrt     : in std_logic;
      adr     : in std_logic_vector(15 downto 0);
      dbi     : out std_logic_vector(7 downto 0);
      dbo     : in std_logic_vector(7 downto 0)
    );
  end component;

  -- operation mode
  signal SelfMode    : std_logic;
  signal KeyMode     : std_logic;

  -- dummy signals
  signal CmtIn       : std_logic;
--signal JoyA        : std_logic_vector(5 downto 0);
--signal StrA        : std_logic;
--signal JoyB        : std_logic_vector(5 downto 0);
--signal StrB        : std_logic;
--signal Kana        : std_logic;
  signal Caps        : std_logic;

  -- MSX slot signals
  signal iSltSltsl_n : std_logic;
  signal iSltMerq_n  : std_logic;
  signal iSltIorq_n  : std_logic;
  signal iSltRd_n    : std_logic;
  signal iSltWr_n    : std_logic;
  signal iSltAdr     : std_logic_vector(15 downto 0);
  signal iSltDat     : std_logic_vector(7 downto 0);
  signal dlydbi      : std_logic_vector(7 downto 0);

  -- internal bus signals (common)
  signal clk21m      : std_logic;
  signal cpuclk      : std_logic;
  signal reset       : std_logic;
  signal clkdiv      : std_logic_vector(1 downto 0);
  signal clkena      : std_logic;
  signal req         : std_logic;
  signal ack, iack   : std_logic;
  signal mem         : std_logic;
  signal wrt         : std_logic;
  signal adr         : std_logic_vector(15 downto 0);
  signal dbi         : std_logic_vector(7 downto 0);
  signal dbo         : std_logic_vector(7 downto 0);

  -- expansion slot signals
  signal Dec_FFFF    : std_logic;
  signal ExpDbi      : std_logic_vector(7 downto 0);
  signal ExpSlot0    : std_logic_vector(7 downto 0);
  signal ExpSlot3    : std_logic_vector(7 downto 0);
  signal ExpSlotX    : std_logic_vector(7 downto 0);
  signal PriSltNum   : std_logic_vector(1 downto 0);
  signal ExpSltNum0  : std_logic_vector(1 downto 0);
  signal ExpSltNum3  : std_logic_vector(1 downto 0);
  signal ExpSltNumX  : std_logic_vector(1 downto 0);

  -- exernal SRAM signals
  signal wex, wey    : std_logic;
  signal RamReq      : std_logic;
  signal RamAck      : std_logic;
  signal RamSel      : std_logic;
  signal RamDbi      : std_logic_vector(7 downto 0);
  signal RamAdr      : std_logic_vector(18 downto 0);

  -- slot select signals
  signal slt_bot     : std_logic;
  signal slt_scc     : std_logic;
  signal slt_erm     : std_logic;
  signal slt_map     : std_logic;
  signal jslt_scc    : std_logic;
  signal jslt_mem    : std_logic;
  signal jslt_mmc    : std_logic;

  -- BIOS-ROM signals
  signal RomReq      : std_logic;
  signal rom_main    : std_logic;
  signal rom_opll    : std_logic;
  signal rom_extr    : std_logic;

  -- PPI(8255) signals
  signal PpiReq      : std_logic;
  signal PpiAck      : std_logic;
  signal PpiDbi      : std_logic_vector(7 downto 0);
  signal PpiPortA    : std_logic_vector(7 downto 0);
  signal PpiPortB    : std_logic_vector(7 downto 0);
  signal PpiPortC    : std_logic_vector(7 downto 0);

  -- PSG signals
  signal PsgReq      : std_logic;
  signal PsgAck      : std_logic;
  signal PsgDbi      : std_logic_vector(7 downto 0);
  signal PsgAmp      : std_logic_vector(7 downto 0);

  -- SCC signals
  signal SccReq      : std_logic;
  signal SccAck      : std_logic;
  signal SccDbi      : std_logic_vector(7 downto 0);
  signal SccRam      : std_logic;
  signal SccWrt      : std_logic;
  signal SccAdr      : std_logic_vector(19 downto 0);
  signal SccDbo      : std_logic_vector(7 downto 0);
  signal SccAmpL     : std_logic_vector(7 downto 0);
  signal SccAmpR     : std_logic_vector(7 downto 0);

  -- ESE-RAM signals
  signal ErmReq      : std_logic;
  signal ErmAck      : std_logic;
  signal ErmDbi      : std_logic_vector(7 downto 0);
  signal ErmRam      : std_logic;
  signal ErmWrt      : std_logic;
  signal ErmDbo      : std_logic_vector(7 downto 0);
  signal ErmAdr      : std_logic_vector(19 downto 0);

  -- Mapper RAM signals
  signal MapReq      : std_logic;
  signal MapAck      : std_logic;
  signal MapDbi      : std_logic_vector(7 downto 0);
  signal MapRam      : std_logic;
  signal MapWrt      : std_logic;
  signal MapDbo      : std_logic_vector(7 downto 0);
  signal MapAdr      : std_logic_vector(21 downto 0);

  -- VDP signals
  signal VdpReq      : std_logic;
  signal VdpAck      : std_logic;
  signal VdpDbi      : std_logic_vector(7 downto 0);
  signal VideoSC     : std_logic;
  signal OeVdp_n     : std_logic;
  signal WeVdp_n     : std_logic;
  signal VdpAdr      : std_logic_vector(16 downto 0);
  signal pVdpInt_n   : std_logic;

  -- Sound signals
  signal pSltSndL    : std_logic_vector(5 downto 0);
  signal pSltSndR    : std_logic_vector(5 downto 0);
  signal pSltSound   : std_logic_vector(5 downto 0);

  -- SD/MMC signals
  signal MmcReq      : std_logic;
  signal MMC_CS      : std_logic;
  signal MMC_CK      : std_logic;
  signal MMC_DI      : std_logic;
  signal MMC_DO      : std_logic;
  signal MmcEna      : std_logic;
  signal MmcDbo      : std_logic_vector(7 downto 0);
  signal MmcDbi      : std_logic_vector(7 downto 0);
  signal MmcSeq      : std_logic_vector(4 downto 0);

  -- ROM signals
  signal RomDbi      : std_logic_vector(7 downto 0);

  -- RTC signals
  signal RtcReq      : std_logic;
  signal RtcAck      : std_logic;
  signal RtcDbi      : std_logic_vector(7 downto 0);

  -- alias
  alias KeyClick : std_logic is PpiPortC(7);

  -- PS/2 signals
  signal KeyWe   : std_logic;
  signal KeyRow  : std_logic_vector(7 downto 0);
  signal iKeyCol : std_logic_vector(7 downto 0);
  signal oKeyCol : std_logic_vector(7 downto 0);

  signal Ps2Dbi1 : std_logic_vector(7 downto 0);
  signal Ps2Dbi2 : std_logic_vector(7 downto 0);
  signal Ps2Dbi3 : std_logic_vector(7 downto 0);
  signal Ps2Dbi4 : std_logic_vector(7 downto 0);
  signal Ps2Dbi5 : std_logic_vector(7 downto 0);
  signal Ps2Dbi6 : std_logic_vector(7 downto 0);
  
begin

  ----------------------------------------------------------------
  -- dummy pin
  ----------------------------------------------------------------
  pClkEna_n <= '0' when SelfMode = '1' else '1';
  pBusReq_n <= 'Z';
  pSltCs12  <= 'Z';

  pSltM1_n  <= pCpuM1_n   when SelfMode = '1' else 'Z';
--pSltM1_n  <= 'Z';
  pSltRfsh_n<= pCpuRfsh_n when SelfMode = '1' else 'Z';
--pSltRfsh_n<= 'Z';

  pSltWait_n<= 'Z';
--pSltInt_n <= pVdpInt_n  when SelfMode = '1' else 'Z';
  pSltInt_n <= pVdpInt_n;

  pCpuClk     <= CpuClk;
  pVideoSC    <= VideoSc;
--pVideoDLClk <= VideoDLClk;
--pVideoDHClk <= VideoDHClk;

  pKeyCaps    <= 'Z';
  pKeyKana    <= 'Z';
  pKeyY       <= (others => 'Z');
  pKeyX       <= (others => 'Z');
  pJoyA       <= (others => 'Z');
  pStrA       <= 'Z';
  pJoyB       <= (others => 'Z');
  pStrB       <= 'Z';


  ----------------------------------------------------------------
  -- bus control
  ----------------------------------------------------------------
  reset     <= not pSltRst_n;
  clk21m    <= pClk21m;

  ----------------------------------------------------------------
  -- operation mode
  ----------------------------------------------------------------
  process(clk21m)

  begin

    if (clk21m'event and clk21m = '1') then

      if (reset = '1') then
      --SelfMode  <= '0';   -- operation mode : 1=master, 0=slave
        SelfMode  <= pSelf;
      end if;

    end if;

  end process;

  ----------------------------------------------------------------
  -- clock generator
  -- pCpuClk should be independent from reset
  ----------------------------------------------------------------
  process(clk21m)

    variable jc : std_logic_vector(4 downto 0);

  begin

    if (clk21m'event and clk21m = '1') then

      jc(3) := jc(2);
      jc(2) := jc(1);
      jc(1) := jc(0);

      if (jc(3 downto 1) = "010" or jc(3 downto 1) = "101") then
        jc(0) := jc(3);
      else
        jc(0) := not jc(3);
      end if;

      -- CPUCLK : 3.58MHz = 21.48MHz / 6
      if (jc(3 downto 2) = "10") then
        clkena <= '1';
      else
        clkena <= '0';
      end if;
      cpuclk <= jc(3);

       -- prescaler : 21.48MHz / 4
      clkdiv <= clkdiv - 1;

    end if;

  end process;

  ----------------------------------------------------------------
  -- Wait control
  ----------------------------------------------------------------
  process(pSltClk, reset)

    variable jSltIorq_n : std_logic;
    variable kSltIorq_n : std_logic;

  begin

    if (reset = '1') then
      jSltIorq_n   := '1';
      kSltIorq_n   := '1';
      pSltWait_n <= 'Z';
    elsif (pSltClk'event and pSltClk = '1') then
      if (SelfMode = '0') then
        pSltWait_n <= 'Z';
      elsif (jSltIorq_n = '0' and kSltIorq_n = '1' and adr(6 downto 2)  = "00110") then
        pSltWait_n <= '0';
--      pSltWait_n <= 'Z';
      else
        pSltWait_n <= 'Z';
      end if;
      kSltIorq_n := jSltIorq_n;
      jSltIorq_n := pSltIorq_n;

    end if;

  end process;

  ----------------------------------------------------------------
  -- MSX slot access control
  ----------------------------------------------------------------
  process(clk21m, reset)

  begin

    if (reset = '1') then

      iSltSltsl_n <= '1';
      iSltMerq_n  <= '1';
      iSltIorq_n  <= '1';
      iSltRd_n    <= '1';
      iSltWr_n    <= '1';
      iSltAdr     <= (others => '1');
      iSltDat     <= (others => '1');

      iack        <= '0';

      Dec_FFFF    <= '0';
      dlydbi      <= (others => '1');

    elsif (clk21m'event and clk21m = '1') then

      -- MSX slot signals
      if (SelfMode = '0') then
        iSltSltsl_n <= pSltSltsl_n;
      else
        iSltSltsl_n <= '1';
      end if;
      iSltMerq_n  <= pSltMerq_n;
      iSltIorq_n  <= pSltIorq_n;
      iSltRd_n    <= pSltRd_n;
      iSltWr_n    <= pSltWr_n;
      iSltAdr     <= pSltAdr;
      iSltDat     <= pSltDat;

      if (iSltSltsl_n = '1' and iSltMerq_n  = '1' and iSltIorq_n = '1') then
        iack <= '0';
      elsif (ack = '1') then
        iack <= '1';
      end if;

      if (adr = X"FFFF") then
        Dec_FFFF <= '1';
      else
        Dec_FFFF <= '0';
      end if;

      if (mem = '1' and slt_erm = '1') then
        dlydbi <= MmcDbi;
      elsif (mem = '0' and adr(6 downto 2)  = "00010") then
        dlydbi <= VdpDbi;
      elsif (mem = '0' and adr(6 downto 2)  = "00110") then
        dlydbi <= VdpDbi;
      else
        dlydbi <= (others => '1');
      end if;

    end if;

  end process;

  process(clk21m, reset)

  begin

    if (reset = '1') then

      jslt_mmc    <= '0';

    elsif (clk21m'event and clk21m = '0') then

      if (mem = '1' and slt_erm = '1') then
        if (MmcEna = '1' and adr(15 downto 13) = "010") then
pKeyCaps <= '0';
          jslt_mmc <= '1';
        else
pKeyCaps <= 'Z';
          jslt_mmc <= '0';
        end if;
      else
          jslt_mmc <= '0';
      end if;

    end if;

  end process;

  -- internal bus signals
  req <= '1' when (((iSltMerq_n = '0' and SelfMode = '1') or
                    iSltSltsl_n = '0' or iSltIorq_n = '0') and
                    (iSltRd_n = '0' or iSltWr_n = '0') and iack = '0') else '0';
  mem <= iSltIorq_n; -- 1=memory area, 0=i/o area
--wrt <= iSltRd_n;   -- 1=write, 0=read
  wrt <= not iSltWr_n;   -- 1=write, 0=read
  dbo <= iSltDat;    -- CPU data (CPU > device)
  adr <= iSltAdr;    -- CPU address (CPU > device)

  ----------------------------------------------------------------
  -- Slot control
  ----------------------------------------------------------------
  process(clk21m, reset)

  begin

    if (reset = '1') then

      ExpSlot0 <= (others => '0');
      ExpSlot3 <= "00101011";      -- primary slot : page 0 => boot-rom, page 1/2 => ese-mmc, page 3 => mapper
      ExpSlotX <= (others => '0');

    elsif (clk21m'event and clk21m = '1') then

      -- Memory mapped I/O port access on FFFFh ... expansion slot register (master mode)
      if (req = '1' and iSltMerq_n = '0'  and wrt = '1' and adr = X"FFFF") then
        if (PpiPortA(7 downto 6) = "00") then
          ExpSlot0 <= dbo;
        elsif (PpiPortA(7 downto 6) = "11") then
          ExpSlot3 <= dbo;
        end if;
      end if;
      -- Memory mapped I/O port access on FFFFh ... expansion slot register (slave mode)
      if (req = '1' and iSltSltsl_n = '0' and wrt = '1' and adr = X"FFFF") then
        ExpSlotX <= dbo;
      end if;

    end if;

  end process;

  -- expansion slot register read
  ExpDbi <= not ExpSlot0 when SelfMode = '1' and PpiPortA(7 downto 6) = "00" else
            not ExpSlot3 when SelfMode = '1' and PpiPortA(7 downto 6) = "11" else
            not ExpSlotX;

  -- expansion slot number : slot 0 (master mode)
  ExpSltNum0 <= ExpSlot0(1 downto 0) when adr(15 downto 14) = "00" else
                ExpSlot0(3 downto 2) when adr(15 downto 14) = "01" else
                ExpSlot0(5 downto 4) when adr(15 downto 14) = "10" else
                ExpSlot0(7 downto 6);

  -- expansion slot number : slot 3 (master mode)
  ExpSltNum3 <= ExpSlot3(1 downto 0) when adr(15 downto 14) = "00" else
                ExpSlot3(3 downto 2) when adr(15 downto 14) = "01" else
                ExpSlot3(5 downto 4) when adr(15 downto 14) = "10" else
                ExpSlot3(7 downto 6);

  -- expansion slot number : slot X (slave mode)
  ExpSltNumX <= ExpSlotX(1 downto 0) when adr(15 downto 14) = "00" else
                ExpSlotX(3 downto 2) when adr(15 downto 14) = "01" else
                ExpSlotX(5 downto 4) when adr(15 downto 14) = "10" else
                ExpSlotX(7 downto 6);

  -- primary slot number (master mode)
  PriSltNum  <= PpiPortA(1 downto 0)  when adr(15 downto 14) = "00" else
                PpiPortA(3 downto 2)  when adr(15 downto 14) = "01" else
                PpiPortA(5 downto 4)  when adr(15 downto 14) = "10" else
                PpiPortA(7 downto 6);

  ----------------------------------------------------------------
  -- slot / address decode
  ----------------------------------------------------------------
  slt_erm <= '0' when adr(15 downto 14) = "00" or adr(15 downto 14) = "11" else
             '1' when PriSltNum  = "11" and
                      ExpSltNum3 = "10" and SelfMode = '1'                 else
             '1' when ExpSltNumX = "01" and SelfMode = '0'                 else '0';

  -- RamX / RamY access request
  RamReq  <= '0';
  -- RamX / RamY select : 0=RamX(SCC), 1=RamY(ESE-RAM,BIOS,Mapper,VRAM)
  RamSel  <= '0';

  -- access request to components
  VdpReq  <= req when mem = '0' and adr(7 downto 2) = "100010" and SelfMode = '0' else      -- I/O:88-8Bh / VDP(V9958)
             req when mem = '0' and adr(7 downto 3) = "10011" and SelfMode = '1' else '0'; -- I/O:98-9Bh / VDP(V9958)
  PpiReq  <= req when mem = '0' and adr(7 downto 2) = "101010" else '0'; -- I/O:A8-ABh / PPI(8255)
  MmcReq  <= req when mem = '1' and slt_erm = '1' else '0';

  -- access acknowledge from components
  ack     <= req; -- PpiAck, VdpAck

--dbi     <= SccDbi when jslt_scc = '1' else
--           RamDbi when jslt_mem = '1' else
--           dlydbi;

  dbi     <= dlydbi;

  pSltDat    <= dbi when pSltIorq_n  = '0' and pSltRd_n = '0' and SelfMode = '0' and pSltAdr(7 downto 2)= "100010" else
                dbi when pSltIorq_n  = '0' and pSltRd_n = '0' and SelfMode = '1' and pSltAdr(7 downto 2)= "100110" else
                dbi when pSltMerq_n  = '0' and pSltRd_n = '0' and jslt_mmc = '1' else
                (others => 'Z');

  pSltSltsl_n<= 'Z';

  pSltBdir_n <= '0' when pSltIorq_n  = '0' and pSltRd_n = '0' and SelfMode = '0' and pSltAdr(7 downto 2)= "100010" else
                'Z';

  ----------------------------------------------------------------
  -- PPI(8255) / primary-slot, keyboard, 1 bit sound port
  ----------------------------------------------------------------
  process(clk21m, reset)

  begin

    if (reset = '1') then

      PpiPortA <= "11111111"; -- primary slot : page 0 => boot-rom, page 1/2 => ese-mmc, page 3 => mapper

    elsif (clk21m'event and clk21m = '1') then

      -- I/O port access on A8-ABh ... PPI(8255) access
      if (PpiReq = '1') then
        if (wrt = '1' and adr(1 downto 0) = "00") then
          PpiPortA <= dbo;
        end if;
      end if;

    end if;

  end process;

  PpiAck <= PpiReq;

  ----------------------------------------------------------------
  -- SD/MMC memory access
  ----------------------------------------------------------------
  process(clk21m, reset)

    variable MmcTmp : std_logic_vector(7 downto 0);

  begin

    if (reset = '1') then

      MmcEna <= '0';
      MmcDbo <= (others => '1');
      MmcDbi <= (others => '1');
      MmcSeq <= (others => '0');
      MmcTmp := (others => '1');

      MMC_CK <= '0';
      MMC_CS <= '1';
      MMC_DI <= 'Z';

    elsif (clk21m'event and clk21m = '1') then

      -- Memory mapped I/O port access on 6000-67FFh ... SD/MMC enable register
      if (MmcReq = '1' and wrt = '1' and adr(15 downto 11) = "01100") then
        if (dbo(7 downto 6) = "01") then
          MmcEna <= '1';
        else
          MmcEna <= '0';
        end if;
      end if;

      -- Memory mapped I/O port access on 4000-5FFFh ... SD/MMC data register
      if (MmcReq = '1' and adr(15 downto 13) = "010" and MmcEna = '1' and MmcSeq = "00000") then -- 4000-5FFFh
        if (wrt = '1') then
          MmcDbo <= dbo;
        else
          MmcDbo <= (others => '1');
        end if;
        MMC_CS <= adr(12);
        MmcSeq <= "10101";
      elsif (MmcSeq /= "00000") then
        MmcSeq <= MmcSeq - 1;
      end if;

      if (MmcSeq(4 downto 1) < "1010" and MmcSeq(4 downto 1) > "0001") then
        MMC_CK <= MmcSeq(0);
      else
        MMC_CK <= '0';
      end if;

      if (MmcSeq(0) = '0') then
        case MmcSeq(4 downto 1) is
          when "1010" => MMC_DI <= MmcDbo(7);
          when "1001" => MMC_DI <= MmcDbo(6);
          when "1000" => MMC_DI <= MmcDbo(5);
          when "0111" => MMC_DI <= MmcDbo(4);
          when "0110" => MMC_DI <= MmcDbo(3);
          when "0101" => MMC_DI <= MmcDbo(2);
          when "0100" => MMC_DI <= MmcDbo(1);
          when "0011" => MMC_DI <= MmcDbo(0);
          when "0010" => MMC_DI <= '1';
          when "0001" => MMC_DI <= 'Z';
          when others => MMC_DI <= 'Z';
        end case;
      end if;

      if (MmcSeq(0) = '0') then
        case MmcSeq(4 downto 1) is
          when "1001" => MmcTmp(7) := MMC_DO;
          when "1000" => MmcTmp(6) := MMC_DO;
          when "0111" => MmcTmp(5) := MMC_DO;
          when "0110" => MmcTmp(4) := MMC_DO;
          when "0101" => MmcTmp(3) := MMC_DO;
          when "0100" => MmcTmp(2) := MMC_DO;
          when "0011" => MmcTmp(1) := MMC_DO;
          when "0010" => MmcTmp(0) := MMC_DO;
          when "0001" => MmcDbi <= MmcTmp;
          when others => null;
        end case;
      end if;

    end if;

  end process;

--MmcAck <= MmcReq;

  ----------------------------------------------------------------
  -- External memory (512kbytes * 2) access
  ----------------------------------------------------------------
  process(clk21m, reset)

  begin

    if (reset = '1') then

      RamAck <= '0';
      wex    <= '1';
      wey    <= '1';
      RamAdr <= (others => '1');
      RamDbi <= (others => '0');

    elsif (clk21m'event and clk21m = '1') then
        wex <= WeVdp_n;
        wey <= '1';
    end if;

  end process;

  pRamCeX   <= '1' when reset = '0' else '0';
--pRamCeY_n <= '0';

  pRamOeX_n <= OeVdp_n;

--             wrt;

--pRamOeY_n <= OeVdp_n when VideoDLClk = '1' else
--             wrt;

  pRamWeX_n <= wex;
--pRamWeY_n <= wey;

  pRamAdrX  <= "11" & VdpAdr;

-- JP: VDPのみの場合，pRamDatXは vdpモジュールがドライブする
--  pRamDatX <= (others => 'Z')       when VideoDLClk = '1' else
--              dbo when wrt = '1' else (others => 'Z');

  ----------------------------------------------------------------
  -- Connect components
  ----------------------------------------------------------------
  U5 : vdp
    port map(
      clk21m, reset, VdpReq, VdpAck, wrt, adr, VdpDbi, dbo, pVdpInt_n, 
      OeVdp_n, WeVdp_n, VdpAdr, pRamDatX, 
      pVideoR, pVideoG, pVideoB, pVideoHS_n, pVideoVS_n, open, 
      open, open, VideoSC, pVideoSYNC, Reso
--      VideoDHClk, VideoDLClk, VideoSC, pVideoSYNC, Reso
    );

  ----------------------------------------------------------------
  -- 1 bit D/A  control
  ----------------------------------------------------------------
  process(clk21m, reset)

    variable AmpL : std_logic_vector(15 downto 0);
    variable AmpR : std_logic_vector(15 downto 0);
    variable Amp  : std_logic_vector(15 downto 0);
    variable AcuL : std_logic_vector(11 downto 0);
    variable AcuR : std_logic_vector(11 downto 0);
    variable Acu  : std_logic_vector(11 downto 0);

  begin

    if (reset = '1') then

      AmpL := (others => '0');
      AmpR := (others => '0');
      Amp  := (others => '0');
      AcuL := (others => '0');
      AcuR := (others => '0');
      Acu  := (others => '0');
      pSltSndL  <= (others => '0');
      pSltSndR  <= (others => '0');
      pSltSound <= (others => '0');

    elsif (clk21m'event and clk21m = '1') then

      AmpL := (('0' & PsgAmp & "0000000") + (SccAmpL & "00000000")) and "1111111111111111";
      AmpR := (('0' & PsgAmp & "0000000") + (SccAmpR & "00000000")) and "1111111111111111";
      Amp  := (('0' & PsgAmp & "0000000") + ('0' & SccAmpL & "0000000") + ('0' & SccAmpR & "0000000")) and "1111111111111111";

      AcuL := ('0' & AcuL(10 downto 0)) + ('0' & AmpL(10 downto 0));
      AcuR := ('0' & AcuR(10 downto 0)) + ('0' & AmpR(10 downto 0));
      Acu  := ('0' & Acu(10 downto 0)) + ('0' & Amp(10 downto 0));

      pSltSndL <= (AmpL(15) xor KeyClick) & AmpL(14 downto 11) & AcuL(11);
      pSltSndR <= (AmpR(15) xor KeyClick) & AmpR(14 downto 11) & AcuR(11);
      pSltSound <= (Amp(15) xor KeyClick) & Amp(14 downto 11) & Acu(11);

    end if;

  end process;

  pSltSnd_L <= pSltSound;
  pSltSnd_R(5) <= 'Z';
  pSltSnd_R(0) <= 'Z';

  pSltSnd_R(1) <= MMC_CS;
  pSltSnd_R(2) <= MMC_DI;
  pSltSnd_R(3) <= MMC_CK;
  MMC_DO <= pSltSnd_R(4);
  pSltSnd_R(4) <= 'Z';



  ----------------------------------------------------------------
  -- PS/2 keyboard interface
  ----------------------------------------------------------------
  process(clk21m, reset)

    type typPs2Seq is (Ps2Idle, Ps2Rxd, Ps2Txd, Ps2Stop);
    variable Ps2Seq : typPs2Seq;
    variable Ps2Chg : std_logic;
    variable Ps2brk : std_logic;
    variable Ps2xE0 : std_logic;
    variable Ps2xE1 : std_logic;
    variable Ps2Cnt : std_logic_vector(3 downto 0);
    variable Ps2Clk : std_logic_vector(2 downto 0);
    variable Ps2Dat : std_logic_vector(7 downto 0);
    variable Ps2Led : std_logic_vector(8 downto 0);
    variable timout : std_logic_vector(15 downto 0);

    variable Ps2Caps : std_logic;
    variable Ps2Kana : std_logic;
    variable Ps2Paus : std_logic;

    type typMtxSeq is (MtxIdle, MtxRead, MtxWrite);
    variable MtxSeq : typMtxSeq;
    variable MtxPtr : std_logic_vector(7 downto 0);

    type rom_type is array (0 to 511) of std_logic_vector(7 downto 0);
    constant key_table : rom_type := (
        X"FF", X"FF", X"FF", X"17", X"76", X"56", X"66", X"FF", -- 00
        X"FF", X"FF", X"FF", X"FF", X"07", X"37", X"67", X"FF", -- 08
        X"FF", X"26", X"06", X"46", X"16", X"64", X"10", X"FF", -- 10
        X"FF", X"FF", X"75", X"05", X"62", X"45", X"20", X"FF", -- 18
        X"FF", X"03", X"55", X"13", X"23", X"40", X"30", X"FF", -- 20
        X"FF", X"08", X"35", X"33", X"15", X"74", X"50", X"FF", -- 28
        X"FF", X"34", X"72", X"53", X"43", X"65", X"60", X"FF", -- 30
        X"FF", X"FF", X"24", X"73", X"25", X"70", X"01", X"FF", -- 38
        X"FF", X"22", X"04", X"63", X"44", X"00", X"11", X"FF", -- 40
        X"FF", X"32", X"42", X"14", X"71", X"54", X"21", X"FF", -- 48
        X"FF", X"52", X"02", X"FF", X"51", X"31", X"FF", X"FF", -- 50
        X"36", X"06", X"77", X"61", X"FF", X"12", X"FF", X"FF", -- 58
        X"FF", X"FF", X"FF", X"FF", X"1B", X"FF", X"57", X"3B", -- 60
        X"FF", X"49", X"41", X"79", X"2A", X"FF", X"FF", X"FF", -- 68
        X"39", X"7A", X"59", X"0A", X"1A", X"3A", X"27", X"6A", -- 70
        X"FF", X"19", X"69", X"5A", X"09", X"4A", X"FF", X"FF", -- 78
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 80
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 88
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 90
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 98
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- A0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- A8
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- B0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- B8
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- C0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- C8
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- D0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- D8
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- E0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- E8
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- F0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- F8

        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 00
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 08
        X"FF", X"26", X"FF", X"FF", X"16", X"FF", X"FF", X"FF", -- 10
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 18
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 20
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 28
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 30
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 38
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 40
        X"FF", X"FF", X"29", X"FF", X"FF", X"FF", X"FF", X"FF", -- 48
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 50
        X"FF", X"FF", X"77", X"FF", X"FF", X"FF", X"FF", X"FF", -- 58
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 60
        X"FF", X"47", X"FF", X"48", X"18", X"FF", X"FF", X"FF", -- 68
        X"28", X"38", X"68", X"FF", X"78", X"58", X"FF", X"FF", -- 70
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 78
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 80
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 88
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 90
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- 98
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- A0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- A8
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- B0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- B8
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- C0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- C8
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- D0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- D8
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- E0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- E8
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", -- F0
        X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF", X"FF"  -- F8
    );

  begin

    if (reset = '1') then

      Ps2Seq := Ps2Idle;
      Ps2Chg := '0';
      Ps2brk := '0';
      Ps2xE0 := '0';
      Ps2xE1 := '0';
      Ps2Cnt := (others => '0');
      Ps2Clk := (others => '1');
      Ps2Dat := (others => '1');
      timout := (others => '1');
      Ps2Led := (others => '1');

      Ps2Caps := '1';
      Ps2Kana := '1';
      Ps2Paus := '0';
      Paus    <= '0';
      Reso    <= '0';

      MtxSeq := MtxIdle;
      MtxPtr := (others => '0');

      pPs2Clk <= 'Z';
      pPs2Dat <= 'Z';
      pKeyX <= (others => 'Z');

      KeyWe   <= '0';
      KeyRow  <= (others => '0');
      iKeyCol <= (others => '0');

--    Ps2dbi1 <= (others => '1');
--    Ps2dbi2 <= (others => '1');
--    Ps2dbi3 <= (others => '1');
--    Ps2dbi4 <= (others => '1');
--    Ps2dbi5 <= (others => '1');
--    Ps2dbi6 <= (others => '1');

    elsif (clk21m'event and clk21m = '1') then

      -- "Scan table > MSX key-matrix" conversion
      if (clkena = '1') then

        case MtxSeq is
          when MtxIdle =>
            if (Ps2Chg = '1') then 
              MtxSeq := MtxRead;
              pKeyX <= (others => 'Z');
              KeyRow <= "0000" & MtxPtr(3 downto 0);
            else
              for i in 7 downto 0 loop
                if (oKeyCol(i) = '1') then
                  pKeyX(i) <= '0';
                else
                  pKeyX(i) <= 'Z';
                end if;
              end loop;
              KeyRow <= "0000" & PpiPortC(3 downto 0);
            end if;
          when MtxRead  =>
            MtxSeq := MtxWrite;
            KeyWe <= not Ps2xE1;
            iKeyCol <= oKeyCol;
            iKeyCol(conv_integer(MtxPtr(6 downto 4))) <= not Ps2brk;
          when MtxWrite  =>
            MtxSeq := MtxIdle;
            KeyWe <= '0';
            KeyRow <= "0000" & PpiPortC(3 downto 0);
            Ps2Chg := '0';
            Ps2brk := '0';
            Ps2xE0 := '0';
            Ps2xE1 := '0';
          when others =>
            MtxSeq := MtxIdle;
        end case;

      end if;

      -- "PS/2 interface > Scan table" conversion
      if (clkena = '1') then

        if (Ps2Clk = "100") then        -- clk inactive
          Ps2Clk(2) := '0';
          timout := X"01FF";            -- countdown timeout (143us = 279ns x 512clk, exceed 100us)

          if (Ps2Seq = Ps2Idle) then
            pPs2Dat <= 'Z';
            Ps2Seq := Ps2Rxd;
            Ps2Cnt := (others => '0');
          elsif (Ps2Seq = Ps2Txd) then
            if (Ps2Cnt = "1000") then
              Ps2Caps := Caps;
              Ps2Kana := Kana;
              Ps2Paus := Paus;
              Ps2Seq := Ps2Idle;
            end if;
            pPs2Dat <= Ps2Led(0);
            Ps2Led := Ps2Led(0) & Ps2Led(8 downto 1);
            Ps2Dat := '1' & Ps2Dat(7 downto 1);
            Ps2Cnt := Ps2Cnt + 1;
          elsif (Ps2Seq = Ps2Rxd) then
            if (Ps2Cnt = "0111") then
              Ps2Seq := Ps2Stop;
            end if;
            Ps2Dat := pPs2Dat & Ps2Dat(7 downto 1);
            Ps2Cnt := Ps2Cnt + 1;
          elsif (Ps2Seq = Ps2Stop) then
            Ps2Seq := Ps2Idle;
            if (Ps2Dat = X"AA") then    -- BAT code (basic assurance test)
              Ps2Caps := not Caps;
              Ps2Kana := not Kana;
              Ps2Paus := not Paus;
            elsif (Ps2Dat = X"14" and Ps2brk = '0' and Ps2xE0 = '0' and Ps2xE1 = '1') then -- pause/break make
              Paus <= not Paus;         -- CPU pause
            elsif (Ps2Dat = X"7C" and Ps2brk = '0' and Ps2xE0 = '1' and Ps2xE1 = '0') then -- printscreen make
              Reso <= not Reso;         -- toggle screen resolution(15kHz<>31kHz)
--          elsif (Ps2Dat = X"7E" and Ps2brk = '0' and Ps2xE0 = '0' and Ps2xE1 = '0') then -- scroll-lock make
--            null;
            elsif (Ps2Dat = X"F0") then -- break code
              Ps2brk := '1';
            elsif (Ps2Dat = X"E0") then -- extnd code E0
              Ps2xE0 := '1';
            elsif (Ps2Dat = X"E1") then -- extnd code E1 (ignore)
              Ps2xE1 := '1';
            elsif (Ps2Dat = X"FA") then  -- Ack of "EDh" command
              Ps2Seq := Ps2Idle;
            else
              Ps2Chg := '1';
            end if;
--          Ps2Dbi6 <= Ps2Dbi5;
--          Ps2Dbi5 <= Ps2Dbi4;
--          Ps2Dbi4 <= Ps2Dbi3;
--          Ps2Dbi3 <= Ps2Dbi2;
--          Ps2Dbi2 <= Ps2Dbi1;
--          Ps2Dbi1 <= Ps2Dat;
          end if;

        elsif (Ps2Clk = "011") then     -- clk active
          Ps2Clk(2) := '1';
          timout := X"01FF";            -- countdown timeout (143us = 279ns x 512clk, exceed 100us)

        elsif (timout = X"0000") then   -- timeout

          pPs2Dat <= 'Z';
          Ps2Seq := Ps2Idle;            -- to Idle state

          if (Ps2Seq = Ps2Idle and Ps2Clk(2) = '1') then

            if (Ps2Dat = X"FA") then
--            Ps2Dbi6 <= X"00";
--            Ps2Dbi5 <= X"11";
--            Ps2Dbi4 <= X"22";
--            Ps2Dbi3 <= X"33";
--            Ps2Dbi2 <= X"44";
--            Ps2Dbi1 <= X"55";
            end if;

            if (Ps2Dat = X"FA" and Ps2Led = "111101101") then
              Ps2Seq := Ps2Txd;         -- Tx data state
              pPs2Dat <= '0';
              Ps2Led := (Caps xor Kana xor Paus xor '1') & "00000" & (not Caps) & (not Kana) & Paus;
              timout := X"FFFF";        -- countdown timeout (18.3ms = 279ns x 65536clk, exceed 1ms)

            elsif (Caps /= Ps2Caps or Kana /= Ps2Kana or Paus /= Ps2Paus) then
              Ps2Seq := Ps2Txd;         -- Tx data state
              pPs2Dat <= '0';
              Ps2Led := "111101101";    -- Command EDh
              timout := X"FFFF";        -- countdown timeout (18.3ms = 279ns x 65536clk, exceed 1ms)

            end if;
          end if;

        else
          timout := timout - 1;         -- countdown timeout (143us = 279ns x 512clk, exceed 100us)

        end if;

        Ps2Clk(1) := Ps2Clk(0);
        Ps2Clk(0) := pPs2Clk;
        MtxPtr := key_table(conv_integer(Ps2xE0 & Ps2Dat));

      end if;

    end if;

  end process;

  U9 : ram port map(KeyRow, clk21m, KeyWe, iKeyCol, oKeyCol);
  
end rtl;
