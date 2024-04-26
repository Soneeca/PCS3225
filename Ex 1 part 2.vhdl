library ieee;
use ieee.numeric_bit.ALL;
use std.textio.all;

entity rom_arquivo is
port (
  addr : in bit_vector(4 downto 0);
  data : out bit_vector(7 downto 0)
	);
end rom_arquivo;

architecture arch of rom_arquivo is
	constant MEM_DEPTH : integer := 2**5;
	type tipo_memoria is array (0 to MEM_DEPTH-1) of bit_vector(7 downto 0);
	impure function init_mem(mif_file_name : in string) return tipo_memoria is
    	file mif_file : text open read_mode is mif_file_name;
    	variable mif_line : line;
    	variable temp_bv : bit_vector(7 downto 0);
    	variable temp_mem : tipo_memoria;
	begin
    	for i in tipo_memoria'range loop
        readline(mif_file, mif_line);
        read(mif_line, temp_bv);
        temp_mem(i) := temp_bv;
    	end loop;
    	return temp_mem;
	end function;
	signal memoria : tipo_memoria := init_mem("conteudo_rom_ativ_02_carga.dat");
begin
  data <= memoria(to_integer(unsigned(addr)));
end arch;
