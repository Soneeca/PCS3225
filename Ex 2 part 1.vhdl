library ieee;
use ieee.numeric_bit.ALL;

entity reg is 
	generic(wordSize: natural := 4);
	port(
		clock : in  bit;
		reset : in  bit;
		load  : in  bit;
		d	  : in  bit_vector(wordSize-1 downto 0);
		q	  : out bit_vector(wordSize-1 downto 0)
	);
end reg;

architecture arquitetura of reg is
begin
	flipflop: process(clock,reset)
	begin
		if reset = '1' then
			apagador : for i in 0 to wordSize-1 loop 
				q(i) <= '0';
			end loop apagador;
		elsif clock = '1' and clock'event then
			if load = '1' then
				q <= d;
			end if;
		end if;
	end process;
end arquitetura;
