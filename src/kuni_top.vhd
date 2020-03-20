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
    pSltSnd_R   : inout std_logic_vector( 5 downto 0);    -- KuniBoard only
--  pSltSound   : inout std_logic_vector( 5 downto 0);    -- KuniBoard only

    pSelf       : in std_logic;
    pClkEna_n   : out std_logic;                          -- KuniBoard only
    pCpuClk     : out std_logic;                          -- KuniBoard only
    pBusReq_n   : inout std_logic;
    pCpuM1_n    : inout std_logic;                        -- KuniBoard only
    pCpuRfsh_n  : inout std_logic;                        -- KuniBoard only

    pKeyCaps    : out std_logic;    -- Kana lamp : 0=ON, Z=OFF
    pKeyKana    : inout std_logic;    -- Kana lamp : 0=ON, Z=OFF
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
    pVideoSYNC : out std_logic;

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
  signal Reso        : std_logic;

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

--pSltWait_n<= 'Z';
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

--  Reso <= pKeyKana;
  Reso <= '1';

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

      pSltSndL <= (AmpL(15)) & AmpL(14 downto 11) & AcuL(11);
      pSltSndR <= (AmpR(15)) & AmpR(14 downto 11) & AcuR(11);
      pSltSound <= (Amp(15)) & Amp(14 downto 11) & Acu(11);

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

end rtl;
