--
--  vga.vhd
--   VHDL Source of ESE-VDP.
--
--    Copyright (C)2000-2003 Kunihiko Ohnaka
--              All rights reserved.
--
-- JP: 日本語のコメント行は JP:を頭に付ける事にする
-- (Japanese comment line starts with "JP:")
--
-- 13th,Octobre,2003 created by Kunihiko Ohnaka
-- JP: VDPのコアの実装と表示デバイスへの出力を別ソースにした．

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use work.vdp_package.all;

entity vga is
  port(
    -- VDP clock ... 21.477MHz
    clk21m  : in std_logic;
    reset   : in std_logic;

    -- Video Input
    videoRin : in std_logic_vector( 5 downto 0);
    videoGin : in std_logic_vector( 5 downto 0);
    videoBin : in std_logic_vector( 5 downto 0);
    hCounterIn : in std_logic_vector(10 downto 0);
    vCounterIn : in std_logic_vector(10 downto 0);
    
    -- Video Output
    videoRout : out std_logic_vector( 5 downto 0);
    videoGout : out std_logic_vector( 5 downto 0);
    videoBout : out std_logic_vector( 5 downto 0);
    videoHSout_n : out std_logic;
    videoVSout_n : out std_logic
    );
end vga;

architecture rtl of vga is
  -- H counter
  signal videoOut : std_logic;

  -- double buffer signal
  signal xPositionW : std_logic_vector(9 downto 0);
  signal xPositionR : std_logic_vector(9 downto 0);
  signal evenOdd    : std_logic;
  signal we_buf     : std_logic;
  signal dataRout   : std_logic_vector(5 downto 0);
  signal dataGout   : std_logic_vector(5 downto 0);
  signal dataBout   : std_logic_vector(5 downto 0);

  -- DISP_START_X + DISP_WIDTH < CLOCKS_PER_LINE/2 = 684
  constant DISP_WIDTH : integer := 562;  -- 30 + 512 + 20
  constant DISP_START_X : integer := 120;
begin

  videoRout <= dataRout when videoOut = '1' else (others => '0');
  videoGout <= dataGout when videoOut = '1' else (others => '0');
  videoBout <= dataBout when videoOut = '1' else (others => '0');

  dbuf : doublebuf port map(clk21m, xPositionW, xPositionR, evenOdd, we_buf,
                            videoRin, videoGin, videoBin,
                            dataRout, dataGout, dataBout);

  xPositionW <= hCounterIn(10 downto 1) - (CLOCKS_PER_LINE/2 - DISP_WIDTH - 10);
  evenOdd <= vCounterIn(1);
  we_buf <= '1';

  process( clk21m, reset )
  begin
    if (reset = '1') then
      videoHSout_n <= '1';
      videoVSout_n <= '1';
      videoOut <= '0';
      xPositionR <= (others => '0');
    elsif (clk21m'event and clk21m = '1') then

      -- JP:垂直同期信号の生成
      if( (vCounterIn = 3*2) or (vCounterIn = 525+3*2) )then
        videoVSout_n <= '0';
      elsif( (vCounterIn = 6*2) or (vCounterIn = 525+6*2) ) then
        videoVSout_n <= '1';
      end if;

      -- JP:水平同期信号の生成
      if( (hCounterIn = 0) or (hCounterIn = (CLOCKS_PER_LINE/2)) ) then
        videoHSout_n <= '0';
      elsif( (hCounterIn = 40) or (hCounterIn = (CLOCKS_PER_LINE/2) + 40) ) then
        videoHSout_n <= '1';
      end if;

      -- JP:映像信号
      if( (hCounterIn = DISP_START_X) or
          (hCounterIn = DISP_START_X + (CLOCKS_PER_LINE/2)) ) then
        videoOut <= '1';
        xPositionR <= (others => '0');
      elsif( (hCounterIn = DISP_START_X+DISP_WIDTH) or
             (hCounterIn = DISP_START_X+DISP_WIDTH + (CLOCKS_PER_LINE/2)) ) then
        videoOut <= '0';
      else
        xPositionR <= xPositionR + 1;
      end if;

    end if;

  end process;
end rtl;



