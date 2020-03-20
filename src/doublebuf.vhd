--
--  doublebuf.vhd
--  Double Buffered Line Memory.
--
--    Copyright (C)2000-2003 Kunihiko Ohnaka
--              All rights reserved.
-- $Id: linebuf.vhd,v 1.1 2003/03/30 03:43:20 kuni Exp $

-- JP: ダブルバッファリング機能付きラインバッファモジュール．
-- JP: xPositionWに X座標を入れ，weを 1にすると書き込みバッファに
-- JP: 書き込まれる．また，xPositionRに X座標を入れると，読み込み
-- JP: バッファから読み出した色コードが qから出力される．
-- JP: evenOdd信号によって，読み込みバッファと書き込みバッファが
-- JP: 切り替わる．

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use work.vdp_package.all;

entity doublebuf is
   port (
         clk        : in  std_logic;
         xPositionW : in  std_logic_vector(9 downto 0);
         xPositionR : in  std_logic_vector(9 downto 0);
         evenOdd    : in  std_logic;
         we         : in  std_logic;
         dataRin    : in  std_logic_vector(5 downto 0);
         dataGin    : in  std_logic_vector(5 downto 0);
         dataBin    : in  std_logic_vector(5 downto 0);
         dataRout   : out  std_logic_vector(5 downto 0);
         dataGout   : out  std_logic_vector(5 downto 0);
         dataBout   : out  std_logic_vector(5 downto 0)
        );
end doublebuf;

architecture RTL of doublebuf is
  signal we_e : std_logic;
  signal we_o : std_logic;
  signal addr_e : std_logic_vector(9 downto 0);
  signal addr_o : std_logic_vector(9 downto 0);
  signal outR_e : std_logic_vector(5 downto 0);
  signal outG_e : std_logic_vector(5 downto 0);
  signal outB_e : std_logic_vector(5 downto 0);
  signal outR_o : std_logic_vector(5 downto 0);
  signal outG_o : std_logic_vector(5 downto 0);
  signal outB_o : std_logic_vector(5 downto 0);
begin

  bufRe : linebuf port map(addr_e, clk, we_e, dataRin, outR_e);
  bufGe : linebuf port map(addr_e, clk, we_e, dataGin, outG_e);
  bufBe : linebuf port map(addr_e, clk, we_e, dataBin, outB_e);

  bufRo : linebuf port map(addr_o, clk, we_o, dataRin, outR_o);
  bufGo : linebuf port map(addr_o, clk, we_o, dataGin, outG_o);
  bufBo : linebuf port map(addr_o, clk, we_o, dataBin, outB_o);

  we_e <= we when evenOdd = '0' else '0';
  we_o <= we when evenOdd = '1' else '0';
  
  addr_e <= xPositionW when evenOdd = '0' else xPositionR;
  addr_o <= xPositionW when evenOdd = '1' else xPositionR;

  dataRout <= outR_e when evenOdd = '1' else outR_o;
  dataGout <= outG_e when evenOdd = '1' else outG_o;
  dataBout <= outB_e when evenOdd = '1' else outB_o;

end RTL;
