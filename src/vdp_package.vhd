library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

package vdp_package is

  -- VDP ID
  constant VDP_ID : std_logic_vector(4 downto 0) := "00000";  -- V9938
--  constant VDP_ID : std_logic_vector(4 downto 0) := "00001";  -- unknown
--  constant VDP_ID : std_logic_vector(4 downto 0) := "00010";  -- V9958

  -- switch the default display mode (NTSC or VGA)
--  constant DISPLAY_MODE : std_logic := '0';  -- NTSC
  constant DISPLAY_MODE : std_logic := '1';  -- VGA

  constant CLOCKS_PER_LINE : integer := 1368;  -- 342x4

  constant ADJUST0_X_NTSC : std_logic_vector( 6 downto 0) := "0110110";    -- = 220/4;
  constant ADJUST0_X_VGA  : std_logic_vector( 6 downto 0) := "0011011";    -- = 220/4/2;
  constant ADJUST0_Y : std_logic_vector( 6 downto 0) := "0101110";     -- = 3+3+13+26+1 = 46
  constant ADJUST0_Y_212 : std_logic_vector( 6 downto 0) := "0100100"; -- = 3+3+13+16+1 = 36

  component ram
    port(
      address  : IN  std_logic_vector(7 downto 0);
      inclock  : IN  std_logic;
      we       : IN  std_logic;
      data     : IN  std_logic_vector(7 downto 0);
      q        : OUT std_logic_vector(7 downto 0)
      );
  end component;

  component ntsc
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
  end component;

  component vga
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
  end component;

  component doublebuf
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
  end component;

  component linebuf
    port (
         address  : in  std_logic_vector(9 downto 0);
         inclock  : in  std_logic;
         we       : in  std_logic;
         data     : in  std_logic_vector(5 downto 0);
         q        : out std_logic_vector(5 downto 0)
        );
  end component;
  
end vdp_package;
