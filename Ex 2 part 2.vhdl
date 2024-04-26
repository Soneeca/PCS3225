library ieee;
use ieee.numeric_bit.ALL;
use ieee.math_real.CEIL;
use ieee.math_real.LOG2;

entity regfile is 
	generic(
		regn: natural := 32;
		wordSize: natural := 64
	);
	port(
		clock :	 	in  bit;
		reset : 	in  bit;
		regWrite : 	in  bit;
		rr1, rr2, wr: in bit_vector(natural(ceil(log2(real(regn)))) -1 downto 0);
		d	  : in  bit_vector(wordSize-1 downto 0);
		q1, q2: out bit_vector(wordSize-1 downto 0)
	);
end regfile;

architecture arquitetura of regfile is	

	type   registrador is array (0 to regn-1) of bit_vector (wordSize-1 downto 0);

	signal rg : registrador;

begin
          
            flipflop: process(clock, reset) begin
            
            apagador: for z in 0 to wordSize-1 loop
		  		rg(regn-1)(z) <= '0';
            end loop apagador;
            
			if reset = '1' then
				apagador2: for x in 0 to regn-1 loop
					apagador3: for y in 0 to wordSize-1 loop
					  rg(x)(y) <= '0';
        			end loop apagador3;
        		end loop apagador2;
			elsif clock = '1' and clock'event then
				if regWrite = '1' then
					if to_integer(unsigned(wr)) < regn-1 then
						rg(to_integer(unsigned(wr))) <= d;
					end if;
				end if;
			end if;
		end process;	
	q1 <= rg(to_integer(unsigned(rr1)));
	q2 <= rg(to_integer(unsigned(rr2)));
end arquitetura;
