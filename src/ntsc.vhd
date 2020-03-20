--
--  ntsc.vhd
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

entity ntsc is
  port(
    -- VDP clock ... 21.477MHz
    clk21m  : in std_logic;
    reset   : in std_logic;

    -- Video Input
    videoRin : in std_logic_vector( 5 downto 0);
    videoGin : in std_logic_vector( 5 downto 0);
    videoBin : in std_logic_vector( 5 downto 0);
    videoHSin_n : in std_logic;
    videoVSin_n : in std_logic;
    
    -- Video Output
    videoRout : out std_logic_vector( 5 downto 0);
    videoGout : out std_logic_vector( 5 downto 0);
    videoBout : out std_logic_vector( 5 downto 0);
    videoHSout_n : out std_logic;
    videoVSout_n : out std_logic
    );
end ntsc;

architecture rtl of ntsc is
begin

    videoHSout_n <= videoHSin_n;
    videoVSout_n <= videoVSin_n;

    videoRout <= videoRin;
    videoGout <= videoGin;
    videoBout <= videoBin;

end rtl;


    
