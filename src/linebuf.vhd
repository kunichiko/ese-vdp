library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity linebuf is
   port (
         address  : in  std_logic_vector(9 downto 0);
         inclock  : in  std_logic;
         we       : in  std_logic;
         data     : in  std_logic_vector(5 downto 0);
         q        : out std_logic_vector(5 downto 0)
        );
end linebuf;

architecture RTL of linebuf is
--  type Mem is array (639 downto 0) of std_logic_vector(5 downto 0);
  type Mem is array (639 downto 0) of std_logic_vector(3 downto 0);
  signal iMem  : Mem;
  signal iAddress : std_logic_vector(9 downto 0);

  begin

  process (inclock)
  begin
    if (inclock'event and inclock ='1') then
      if (we = '1') then
--        iMem(conv_integer(address)) <= data(5 downto 0);
        iMem(conv_integer(address)) <= data(5 downto 2);
      end if;
      iAddress <= address;
    end if;
  end process;

--  q <= iMem(conv_integer(iAddress));
  q <= iMem(conv_integer(iAddress)) & "00";

end RTL;
