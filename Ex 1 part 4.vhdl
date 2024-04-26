library ieee;
use ieee.numeric_bit.ALL;

entity ram is
	generic(
      		addressSize : natural := 5;
      		wordSize : natural := 8
);
	port (
      		ck, wr : in bit;
      		addr : in bit_vector(addressSize-1 downto 0);
      		data_i : in bit_vector(wordSize-1 downto 0);
      		data_o : out bit_vector(wordSize-1 downto 0)
);
end ram;

architecture arch of ram is
	constant MEM_DEPTH : integer := 2**addressSize;
	type tipo_memoria is array (0 to MEM_DEPTH-1) of bit_vector(wordSize-1 downto 0);
	signal memoria : tipo_memoria;
    begin
	write: process(ck, wr)
		begin
			if(rising_edge(ck) and wr='1')
			then memoria(to_integer(unsigned(addr))) <= data_i;
			end if;
		end process write;
	read: process(addr, data_i)
		begin
			data_o <= memoria(to_integer(unsigned(addr)));
		end process read;
end arch;
