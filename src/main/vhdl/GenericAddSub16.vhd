--------------------------------------------------------------------------------
--                Xilinx_GenericAddSub_16_fixed_01_slice4_init
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
-- >> University of Kassel, Germany
-- >> Digital Technology Group
-- >> Author(s):
-- >> Marco Kleinlein <kleinlein@uni-kassel.de>
--------------------------------------------------------------------------------
library UNISIM;
use UNISIM.Vcomponents.all;
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Xilinx_GenericAddSub_16_fixed_01_slice4_init is
   port ( x_in : in  std_logic_vector(3 downto 0);
          y_in : in  std_logic_vector(3 downto 0);
          neg_x_in : in  std_logic;
          neg_y_in : in  std_logic;
          carry_in : in  std_logic;
          carry_out : out  std_logic;
          sum_out : out  std_logic_vector(3 downto 0)   );
end entity;

architecture arch of Xilinx_GenericAddSub_16_fixed_01_slice4_init is
signal cc_di :  std_logic_vector(3 downto 0);
signal cc_s :  std_logic_vector(3 downto 0);
signal cc_o :  std_logic_vector(3 downto 0);
signal cc_co :  std_logic_vector(3 downto 0);
signal lut_o5 :  std_logic_vector(3 downto 0);
signal lut_o6 :  std_logic_vector(3 downto 0);
begin
   cc_di <= lut_o5;
   cc_s <= lut_o6;
   lut_bit_0: LUT6_2
      generic map ( init => x"099609960a5a0a5a")
      port map ( i0 => y_in(0),
                 i1 => x_in(0),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => '1',
                 i5 => '1',
                 o5 => lut_o5(0),
                 o6 => lut_o6(0));
   lut_bit_1: LUT6_2
      generic map ( init => x"099609960a5a0a5a")
      port map ( i0 => y_in(1),
                 i1 => x_in(1),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => '1',
                 i5 => '1',
                 o5 => lut_o5(1),
                 o6 => lut_o6(1));
   lut_bit_2: LUT6_2
      generic map ( init => x"099609960a5a0a5a")
      port map ( i0 => y_in(2),
                 i1 => x_in(2),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => '1',
                 i5 => '1',
                 o5 => lut_o5(2),
                 o6 => lut_o6(2));
   lut_bit_3: LUT6_2
      generic map ( init => x"099609960a5a0a5a")
      port map ( i0 => y_in(3),
                 i1 => x_in(3),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => '1',
                 i5 => '1',
                 o5 => lut_o5(3),
                 o6 => lut_o6(3));
   slice_cc: CARRY4
      port map ( ci => carry_in,
                 co => cc_co,
                 cyinit => '0',
                 di => cc_di,
                 o => cc_o,
                 s => cc_s);
carry_out <= cc_co(3);
sum_out <= cc_o(3 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                  Xilinx_GenericAddSub_16_fixed_01_slice4
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
-- >> University of Kassel, Germany
-- >> Digital Technology Group
-- >> Author(s):
-- >> Marco Kleinlein <kleinlein@uni-kassel.de>
--------------------------------------------------------------------------------
library UNISIM;
use UNISIM.Vcomponents.all;
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Xilinx_GenericAddSub_16_fixed_01_slice4 is
   port ( x_in : in  std_logic_vector(3 downto 0);
          y_in : in  std_logic_vector(3 downto 0);
          neg_x_in : in  std_logic;
          neg_y_in : in  std_logic;
          carry_in : in  std_logic;
          carry_out : out  std_logic;
          sum_out : out  std_logic_vector(3 downto 0)   );
end entity;

architecture arch of Xilinx_GenericAddSub_16_fixed_01_slice4 is
signal cc_di :  std_logic_vector(3 downto 0);
signal cc_s :  std_logic_vector(3 downto 0);
signal cc_o :  std_logic_vector(3 downto 0);
signal cc_co :  std_logic_vector(3 downto 0);
signal lut_o5 :  std_logic_vector(3 downto 0);
signal lut_o6 :  std_logic_vector(3 downto 0);
begin
   cc_di <= lut_o5;
   cc_s <= lut_o6;
   lut_bit_0: LUT6_2
      generic map ( init => x"099609960a5a0a5a")
      port map ( i0 => y_in(0),
                 i1 => x_in(0),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => '1',
                 i5 => '1',
                 o5 => lut_o5(0),
                 o6 => lut_o6(0));
   lut_bit_1: LUT6_2
      generic map ( init => x"099609960a5a0a5a")
      port map ( i0 => y_in(1),
                 i1 => x_in(1),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => '1',
                 i5 => '1',
                 o5 => lut_o5(1),
                 o6 => lut_o6(1));
   lut_bit_2: LUT6_2
      generic map ( init => x"099609960a5a0a5a")
      port map ( i0 => y_in(2),
                 i1 => x_in(2),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => '1',
                 i5 => '1',
                 o5 => lut_o5(2),
                 o6 => lut_o6(2));
   lut_bit_3: LUT6_2
      generic map ( init => x"099609960a5a0a5a")
      port map ( i0 => y_in(3),
                 i1 => x_in(3),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => '1',
                 i5 => '1',
                 o5 => lut_o5(3),
                 o6 => lut_o6(3));
   slice_cc: CARRY4
      port map ( ci => carry_in,
                 co => cc_co,
                 cyinit => '0',
                 di => cc_di,
                 o => cc_o,
                 s => cc_s);
carry_out <= cc_co(3);
sum_out <= cc_o(3 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                      Xilinx_GenericAddSub_16_fixed_01
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
-- >> University of Kassel, Germany
-- >> Digital Technology Group
-- >> Author(s):
-- >> Marco Kleinlein <kleinlein@uni-kassel.de>
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Xilinx_GenericAddSub_16_fixed_01 is
   port ( x_i : in  std_logic_vector(15 downto 0);
          y_i : in  std_logic_vector(15 downto 0);
          sum_o : out  std_logic_vector(15 downto 0);
          c_o : out  std_logic   );
end entity;

architecture arch of Xilinx_GenericAddSub_16_fixed_01 is
   component Xilinx_GenericAddSub_16_fixed_01_slice4_init is
      port ( x_in : in  std_logic_vector(3 downto 0);
             y_in : in  std_logic_vector(3 downto 0);
             neg_x_in : in  std_logic;
             neg_y_in : in  std_logic;
             carry_in : in  std_logic;
             carry_out : out  std_logic;
             sum_out : out  std_logic_vector(3 downto 0)   );
   end component;

   component Xilinx_GenericAddSub_16_fixed_01_slice4 is
      port ( x_in : in  std_logic_vector(3 downto 0);
             y_in : in  std_logic_vector(3 downto 0);
             neg_x_in : in  std_logic;
             neg_y_in : in  std_logic;
             carry_in : in  std_logic;
             carry_out : out  std_logic;
             sum_out : out  std_logic_vector(3 downto 0)   );
   end component;

signal carry_0, carry_1, carry_2, carry_3 :  std_logic;
signal sum_t :  std_logic_vector(15 downto 0);
signal x :  std_logic_vector(15 downto 0);
signal y :  std_logic_vector(15 downto 0);
begin
   x(15 downto 0) <= x_i;
   y(15 downto 0) <= y_i;
   slice_0: Xilinx_GenericAddSub_16_fixed_01_slice4_init
      port map ( carry_in => '1',
                 carry_out => carry_0,
                 neg_x_in => '0',
                 neg_y_in => '1',
                 sum_out => sum_t(3 downto 0),
                 x_in => x(3 downto 0),
                 y_in => y(3 downto 0));
   slice_1: Xilinx_GenericAddSub_16_fixed_01_slice4
      port map ( carry_in => carry_0,
                 carry_out => carry_1,
                 neg_x_in => '0',
                 neg_y_in => '1',
                 sum_out => sum_t(7 downto 4),
                 x_in => x(7 downto 4),
                 y_in => y(7 downto 4));
   slice_2: Xilinx_GenericAddSub_16_fixed_01_slice4
      port map ( carry_in => carry_1,
                 carry_out => carry_2,
                 neg_x_in => '0',
                 neg_y_in => '1',
                 sum_out => sum_t(11 downto 8),
                 x_in => x(11 downto 8),
                 y_in => y(11 downto 8));
   slice_3: Xilinx_GenericAddSub_16_fixed_01_slice4
      port map ( carry_in => carry_2,
                 carry_out => carry_3,
                 neg_x_in => '0',
                 neg_y_in => '1',
                 sum_out => sum_t(15 downto 12),
                 x_in => x(15 downto 12),
                 y_in => y(15 downto 12));
   sum_o <= sum_t(15 downto 0);
   c_o <= carry_3;
end architecture;

--------------------------------------------------------------------------------
--                Xilinx_GenericAddSub_16_dss_slice4_dss_init
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
-- >> University of Kassel, Germany
-- >> Digital Technology Group
-- >> Author(s):
-- >> Marco Kleinlein <kleinlein@uni-kassel.de>
--------------------------------------------------------------------------------
library UNISIM;
use UNISIM.Vcomponents.all;
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Xilinx_GenericAddSub_16_dss_slice4_dss_init is
   port ( x_in : in  std_logic_vector(3 downto 0);
          y_in : in  std_logic_vector(3 downto 0);
          neg_x_in : in  std_logic;
          neg_y_in : in  std_logic;
          carry_in : in  std_logic;
          carry_out : out  std_logic;
          sum_out : out  std_logic_vector(3 downto 0);
          bbus_in : in  std_logic_vector(3 downto 0);
          bbus_out : out  std_logic_vector(3 downto 0)   );
end entity;

architecture arch of Xilinx_GenericAddSub_16_dss_slice4_dss_init is
signal cc_di :  std_logic_vector(3 downto 0);
signal cc_s :  std_logic_vector(3 downto 0);
signal cc_o :  std_logic_vector(3 downto 0);
signal cc_co :  std_logic_vector(3 downto 0);
signal lut_o5 :  std_logic_vector(3 downto 0);
signal lut_o6 :  std_logic_vector(3 downto 0);
signal bb_t :  std_logic_vector(3 downto 0);
begin
   cc_di <= bbus_in;
   cc_s <= lut_o6;
   lut_bit_0: LUT6_2
      generic map ( init => x"666666661bd81bd8")
      port map ( i0 => y_in(0),
                 i1 => x_in(0),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => bbus_in(0),
                 i5 => '1',
                 o5 => bb_t(0),
                 o6 => lut_o6(0));
   lut_bit_1: LUT6_2
      generic map ( init => x"6669999672487248")
      port map ( i0 => y_in(1),
                 i1 => x_in(1),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => bbus_in(1),
                 i5 => '1',
                 o5 => bb_t(1),
                 o6 => lut_o6(1));
   lut_bit_2: LUT6_2
      generic map ( init => x"9669699612481248")
      port map ( i0 => y_in(2),
                 i1 => x_in(2),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => bbus_in(2),
                 i5 => '1',
                 o5 => bb_t(2),
                 o6 => lut_o6(2));
   lut_bit_3: LUT6_2
      generic map ( init => x"9669699612481248")
      port map ( i0 => y_in(3),
                 i1 => x_in(3),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => bbus_in(3),
                 i5 => '1',
                 o5 => bb_t(3),
                 o6 => lut_o6(3));
   slice_cc: CARRY4
      port map ( ci => carry_in,
                 co => cc_co,
                 cyinit => '0',
                 di => cc_di,
                 o => cc_o,
                 s => cc_s);
carry_out <= cc_co(3);
sum_out <= cc_o(3 downto 0);
bbus_out <= bb_t;
end architecture;

--------------------------------------------------------------------------------
--                   Xilinx_GenericAddSub_16_dss_slice4_dss
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
-- >> University of Kassel, Germany
-- >> Digital Technology Group
-- >> Author(s):
-- >> Marco Kleinlein <kleinlein@uni-kassel.de>
--------------------------------------------------------------------------------
library UNISIM;
use UNISIM.Vcomponents.all;
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Xilinx_GenericAddSub_16_dss_slice4_dss is
   port ( x_in : in  std_logic_vector(3 downto 0);
          y_in : in  std_logic_vector(3 downto 0);
          neg_x_in : in  std_logic;
          neg_y_in : in  std_logic;
          carry_in : in  std_logic;
          carry_out : out  std_logic;
          sum_out : out  std_logic_vector(3 downto 0);
          bbus_in : in  std_logic_vector(3 downto 0);
          bbus_out : out  std_logic_vector(3 downto 0)   );
end entity;

architecture arch of Xilinx_GenericAddSub_16_dss_slice4_dss is
signal cc_di :  std_logic_vector(3 downto 0);
signal cc_s :  std_logic_vector(3 downto 0);
signal cc_o :  std_logic_vector(3 downto 0);
signal cc_co :  std_logic_vector(3 downto 0);
signal lut_o5 :  std_logic_vector(3 downto 0);
signal lut_o6 :  std_logic_vector(3 downto 0);
signal bb_t :  std_logic_vector(3 downto 0);
begin
   cc_di <= bbus_in;
   cc_s <= lut_o6;
   lut_bit_0: LUT6_2
      generic map ( init => x"9669699612481248")
      port map ( i0 => y_in(0),
                 i1 => x_in(0),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => bbus_in(0),
                 i5 => '1',
                 o5 => bb_t(0),
                 o6 => lut_o6(0));
   lut_bit_1: LUT6_2
      generic map ( init => x"9669699612481248")
      port map ( i0 => y_in(1),
                 i1 => x_in(1),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => bbus_in(1),
                 i5 => '1',
                 o5 => bb_t(1),
                 o6 => lut_o6(1));
   lut_bit_2: LUT6_2
      generic map ( init => x"9669699612481248")
      port map ( i0 => y_in(2),
                 i1 => x_in(2),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => bbus_in(2),
                 i5 => '1',
                 o5 => bb_t(2),
                 o6 => lut_o6(2));
   lut_bit_3: LUT6_2
      generic map ( init => x"9669699612481248")
      port map ( i0 => y_in(3),
                 i1 => x_in(3),
                 i2 => neg_y_in,
                 i3 => neg_x_in,
                 i4 => bbus_in(3),
                 i5 => '1',
                 o5 => bb_t(3),
                 o6 => lut_o6(3));
   slice_cc: CARRY4
      port map ( ci => carry_in,
                 co => cc_co,
                 cyinit => '0',
                 di => cc_di,
                 o => cc_o,
                 s => cc_s);
carry_out <= cc_co(3);
sum_out <= cc_o(3 downto 0);
bbus_out <= bb_t;
end architecture;

--------------------------------------------------------------------------------
--                        Xilinx_GenericAddSub_16_dss
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: 
-- >> University of Kassel, Germany
-- >> Digital Technology Group
-- >> Author(s):
-- >> Marco Kleinlein <kleinlein@uni-kassel.de>
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Xilinx_GenericAddSub_16_dss is
   port ( x_i : in  std_logic_vector(15 downto 0);
          y_i : in  std_logic_vector(15 downto 0);
          neg_x_i : in  std_logic;
          neg_y_i : in  std_logic;
          sum_o : out  std_logic_vector(15 downto 0);
          c_o : out  std_logic   );
end entity;

architecture arch of Xilinx_GenericAddSub_16_dss is
   component Xilinx_GenericAddSub_16_dss_slice4_dss_init is
      port ( x_in : in  std_logic_vector(3 downto 0);
             y_in : in  std_logic_vector(3 downto 0);
             neg_x_in : in  std_logic;
             neg_y_in : in  std_logic;
             carry_in : in  std_logic;
             carry_out : out  std_logic;
             sum_out : out  std_logic_vector(3 downto 0);
             bbus_in : in  std_logic_vector(3 downto 0);
             bbus_out : out  std_logic_vector(3 downto 0)   );
   end component;

   component Xilinx_GenericAddSub_16_dss_slice4_dss is
      port ( x_in : in  std_logic_vector(3 downto 0);
             y_in : in  std_logic_vector(3 downto 0);
             neg_x_in : in  std_logic;
             neg_y_in : in  std_logic;
             carry_in : in  std_logic;
             carry_out : out  std_logic;
             sum_out : out  std_logic_vector(3 downto 0);
             bbus_in : in  std_logic_vector(3 downto 0);
             bbus_out : out  std_logic_vector(3 downto 0)   );
   end component;

signal carry :  std_logic_vector(4 downto 0);
signal sum_t :  std_logic_vector(15 downto 0);
signal x :  std_logic_vector(15 downto 0);
signal y :  std_logic_vector(15 downto 0);
signal neg_x :  std_logic;
signal neg_y :  std_logic;
signal bbus :  std_logic_vector(16 downto 0);
begin
   x(15 downto 0) <= x_i;
   y(15 downto 0) <= y_i;
   neg_x <= neg_x_i;
   neg_y <= neg_y_i;
   bbus(0) <= '0';
   slice_0: Xilinx_GenericAddSub_16_dss_slice4_dss_init
      port map ( bbus_in => bbus(3 downto 0),
                 bbus_out => bbus(4 downto 1),
                 carry_in => '0',
                 carry_out => carry(0),
                 neg_x_in => neg_x,
                 neg_y_in => neg_y,
                 sum_out => sum_t(3 downto 0),
                 x_in => x(3 downto 0),
                 y_in => y(3 downto 0));
   slice_1: Xilinx_GenericAddSub_16_dss_slice4_dss
      port map ( bbus_in => bbus(7 downto 4),
                 bbus_out => bbus(8 downto 5),
                 carry_in => carry(0),
                 carry_out => carry(1),
                 neg_x_in => neg_x,
                 neg_y_in => neg_y,
                 sum_out => sum_t(7 downto 4),
                 x_in => x(7 downto 4),
                 y_in => y(7 downto 4));
   slice_2: Xilinx_GenericAddSub_16_dss_slice4_dss
      port map ( bbus_in => bbus(11 downto 8),
                 bbus_out => bbus(12 downto 9),
                 carry_in => carry(1),
                 carry_out => carry(2),
                 neg_x_in => neg_x,
                 neg_y_in => neg_y,
                 sum_out => sum_t(11 downto 8),
                 x_in => x(11 downto 8),
                 y_in => y(11 downto 8));
   slice_3: Xilinx_GenericAddSub_16_dss_slice4_dss
      port map ( bbus_in => bbus(15 downto 12),
                 bbus_out => bbus(16 downto 13),
                 carry_in => carry(2),
                 carry_out => carry(3),
                 neg_x_in => neg_x,
                 neg_y_in => neg_y,
                 sum_out => sum_t(15 downto 12),
                 x_in => x(15 downto 12),
                 y_in => y(15 downto 12));
   sum_o <= sum_t(15 downto 0);
   c_o <= carry(4);
end architecture;
