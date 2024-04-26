library ieee;
use ieee.numeric_bit.ALL;

entity fulladder is
  port (
    a, b, cin: in bit;
    s, cout: out bit
  );
 end entity;

architecture structural of fulladder is
  signal axorb: bit;
begin
  axorb <= a xor b;
  s <= axorb xor cin;
  cout <= (axorb and cin) or (a and b);
end architecture;

library ieee;
use ieee.numeric_bit.ALL;

entity alu1bit is
port (
a, b, less , cin: in bit;
result, cout, set, overflow: out bit;
ainvert , binvert : in bit ;
operation: in bit_vector(1 downto 0)
);
end entity;

architecture arch of alu1bit is
  signal aalu, balu, andalu, oralu, addalu, coutalu: bit;
  component fulladder is
  port(
  	a, b, cin: in bit;
    s, cout: out bit
  );
  end component fulladder;
begin
  with ainvert select
  aalu <= a      when '0',
         not(a) when '1';
         
  with binvert select
  balu <= b      when '0',
         not(b) when '1';
         
  andalu <= aalu and balu;
  oralu  <= aalu or balu;
  
  adder: fulladder port map(
  	a => aalu,
    b => balu,
    cin => cin,
    cout => coutalu,
    s => addalu
  );
  
  cout <= coutalu;
  set  <= addalu;
  overflow <= coutalu xor cin;
  
  with operation select
  result <= andalu when "00",
            oralu  when "01",
            addalu when "10",
            b when "11";
            
end arch;

entity alu is
	generic (
		size: natural := 64
	);
	port (
		A,B	: in  bit_vector(size-1 downto 0);
		F	: out  bit_vector(size-1 downto 0);
		S	: in  bit_vector(3 downto 0);
		Z	: out bit;
		Ov	: out bit;
		Co	: out bit
	);
end entity alu;

architecture alu_arch of alu is
	component alu1bit is
	port(
		a, b, less, cin: in bit;
		result, cout, set, overflow: out bit;
		ainvert, binvert: in bit;
		operation : in bit_vector (1 downto 0)
	);
	end component;
	signal cvs : bit_vector(size downto 0);
	signal menor, setar, overflo, resultado : bit_vector(size-1 downto 0);
	signal operador: bit_vector (1 downto 0);
	signal ainv, binv : bit; 
	
begin 
	
	process(S) is
	begin
		if 		S = "0000" then
			operador <= "00";
			ainv <= '0';
			binv <= '0';
			cvs(0) <= '0';
		elsif 	S = "0001" then
		    operador <= "01";
		    ainv <= '0';
			binv <= '0';
			cvs(0) <= '0';
		elsif	S = "0010" then
			operador <= "10";
			ainv <= '0';
			binv <= '0';
			cvs(0) <= '0';
		elsif	S = "0110" then
			operador <= "10";
			ainv <= '0';
			binv <= '1';
			cvs(0) <= '1';
		elsif 	S = "0111" then
			operador <= "11";
			ainv <= '0';
			binv <= '1';
			cvs(0) <= '1';
		elsif 	S = "1100" then
			operador <= "00";
			ainv <= '1';
			binv <= '1';
			cvs(0) <= '1'; 
		end if;
	end process;
    
    process (menor,setar)
	begin
   		menore: FOR i IN 0 to size-1 LOOP
			if i = 0 then
				menor(i) <= setar(size-1);
			else
				menor(i) <= '0';
			end if;
		END LOOP menore;
	end process;
	
	alugeral: FOR i IN size-1 downto 0 GENERATE
		alu1i : alu1bit port map(
		A(i),
		B(i),
		menor(i),
		cvs(i),
		resultado(i),
		cvs(i+1),
		setar(i),
		overflo(i),
		ainv,
		binv,
		operador 
		);
	END GENERATE alugeral;
	F <= resultado;
	Ov <= overflo(size-1);
	Z  <= '1' when resultado =(resultado'range => '0') else '0';
	Co <= cvs(size); 
	
end alu_arch;

entity signExtend is
	port(
      i: in		bit_vector(31 downto 0);
      o: out	bit_vector(63 downto 0)
);
end signExtend;

architecture achext of signExtend is
signal d: bit_vector (10 downto 0);
signal c: bit_vector(7 downto 0);
signal b: bit_vector (5 downto 0);
signal tipo : bit_vector(1 downto 0);
signal de : bit_vector(8 downto 0);
signal ce: bit_vector (18 downto 0);
signal be : bit_vector (25 downto 0);
signal r_out, ldur, stur, cbz, bo : bit_vector(63 downto 0);
	begin
    d <= i(31 downto 21);
	c <= i(31 downto 24);
	b <= i(31 downto 26);
    de <= i(20 downto 12);
	ce <= i(23 downto 5);
	be <= i(25 downto 0);
    se : process(d, cbz, b) is
	begin
		if		d = "11111000010" then tipo <= "00";
		elsif 	d = "11111000000" then tipo <= "01";
		elsif   c = "10110100" 	  then tipo <= "10";
		elsif 	b = "000101" 	  then tipo <= "11";
		end if;
	end process;

des : process(de) is
	begin
      ldur(8 downto 0) <= de;
      stur(8 downto 0) <= de;
		if de(8) = '0' then
			for k in 63 downto 9 loop
          ldur(k) <= '0';
      stur(k) <= '0';
    end loop;
		else
			for k in 63 downto 9 loop
          ldur(k) <= '1';
      stur(k) <= '1';
end loop;
		end if;
	end process;
	
ces : process(ce) is
	begin
      cbz(18 downto 0) <= ce;
		if ce(18) = '0' then
			for k in 63 downto 19 loop
              cbz(k) <= '0';
    end loop;
		else
			for k in 63 downto 19 loop
              cbz(k) <= '1';
end loop;
		end if;
	end process;
	
bes : process(be) is
	begin
      bo(25 downto 0) <= be;
		if be(25) = '0' then
			for k in 63 downto 26 loop
				bo(k) <= '0';
    end loop;
		else
			for k in 63 downto 26 loop
				bo(k) <= '1';
end loop;
		end if;
	end process;	
with tipo select
 o <= 	ldur when "00",
		stur when "01",
		cbz when "10",
		bo when "11";
end achext;

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

library ieee;
use ieee.numeric_bit.ALL;

entity PC is 
generic(wordSize: natural := 64);
	port(
		clock : in  bit;
		reset : in  bit;
		load  : in  bit;
		d	  : in  bit_vector(wordSize-1 downto 0);
		q	  : out bit_vector(wordSize-1 downto 0)
	);
end PC;

architecture arquitetura of PC is
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

library ieee;
use ieee.numeric_bit.ALL;

entity shiftLeft2 is 
	generic (
		wordSize : natural := 64
	);
	port(
		I: in bit_vector(wordSize-1 downto 0);
		O: out bit_vector(wordSize-1 downto 0)
	);
end shiftLeft2;

architecture arque of shiftLeft2 is
	begin
		o <= i(wordSize-3 downto 0) & "00";
end arque;

library ieee;
use ieee.numeric_bit.ALL;

entity controlunit is
	port(
		reg2loc: out bit;
		uncondbranch: out bit;
		branch : out bit;
		memRead : out bit;
		memToReg: out bit;
		aluOp : out bit_vector(1 downto 0);
		memWrite: out bit;
		aluSrc : out bit;
		regWrite : out bit;
		opcode: in bit_vector(10 downto 0)
	);
end entity;
architecture acu of controlunit is
	begin
	acup: process (opcode) is
	begin
		if opcode = "11111000010" then
		reg2loc <= '0';uncondbranch <= '0';branch <= '0';memRead <= '1';memToReg<= '1';aluop <= "00";memWrite <= '0';aluSrc	<= '1';regWrite	<= '1';
		elsif opcode = "11111000000" then
		reg2loc <= '1';uncondbranch <= '0';branch 	<= '0';memRead <= '0';memToReg <= '0';aluop <= "00";memWrite <= '1';aluSrc <= '1';regWrite <= '0';
		elsif opcode(10 downto 3) = "10110100" then
		reg2loc	<= '1';uncondbranch <= '0';branch	<= '1';memRead <= '0';memToReg <= '0';aluop <= "01";memWrite <= '0';aluSrc <= '0';regWrite <= '0';
		elsif opcode(10 downto 5) = "000101" then
		reg2loc <= '1';uncondbranch <= '1';branch <= '1';memRead <= '0';memToReg <= '0';aluop <= "01";memWrite <= '0';aluSrc	<= '0';regWrite <= '0';
		elsif opcode = "10001011000" or opcode = "11001011000" or opcode = "10001010000" or opcode = "10101010000" then
		reg2loc <= '0';uncondbranch <= '0';branch 	<= '0';memRead <= '0';memToReg <= '0';aluop <= "10";memWrite <= '0';aluSrc <= '0';regWrite <= '1';
		end if;
	end process;
end acu;

library ieee;
use ieee.numeric_bit.ALL;

entity alucontrol is
  port (
    aluop:   in  bit_vector(1 downto 0);
    opcode:  in  bit_vector(10 downto 0);
    aluCtrl: out bit_vector(3 downto 0)
  );
end entity;
architecture acontrol of alucontrol is
begin
	process(opcode, aluop) is
	begin
		if    aluop = "00" then aluCtrl <= "0010";
		elsif aluop = "01" then aluCtrl <= "0111";
		elsif aluop = "10" then
			if    opcode = "10001011000" then aluCtrl <= "0010";
			elsif opcode = "11001011000" then aluCtrl <= "0110";
			elsif opcode = "10001010000" then aluCtrl <= "0000";
			elsif opcode = "10101010000" then aluCtrl <= "0001";
			end if;
		end if;
	end process;
end acontrol;

library ieee;
use ieee.numeric_bit.ALL;

entity mux is
	generic(
		wordSize: natural := 64
	);
	port(
		sel: in bit;
		A: in bit_vector(wordSize -1 downto 0);
		B: in bit_vector(wordSize -1 downto 0);
		O: out bit_vector(wordSize -1 downto 0)
	);
end mux;

architecture archmx of mux is
	begin
		with sel select
		  O <= A when '0',
			   B when '1';
		
end archmx;

library ieee;
use ieee.numeric_bit.ALL;
use ieee.math_real.ALL;

entity datapath is
	generic(
		wordSize: natural := 64
	);
	port(
		clock: in bit;
		reset: in bit;
		reg2loc: in bit;
		pcsrc: in bit;
		memToReg: in bit;
		aluCtrl: in bit_vector(3 downto 0);
		aluSrc: in bit;
		regWrite: in bit;
		opcode: out bit_vector(10 downto 0);
		zero: out bit;
		imAddr: out bit_vector(63 downto 0);
		imOut: in bit_vector(31 downto 0);
		dmAddr: out bit_vector(63 downto 0);
		dmIn: out bit_vector(63 downto 0);
		dmOut: in bit_vector(63 downto 0)
	);
end datapath;

architecture arquite of datapath is 

	component alu is
		generic (
			size: natural := 64
		);
		port (
			A,B	: in  bit_vector(size-1 downto 0);
			F	: out  bit_vector(size-1 downto 0);
			S	: in  bit_vector(3 downto 0);
			Z	: out bit;
			Ov	: out bit;
			Co	: out bit
		);
	end component;

	component signExtend is
		port (
			i : in  bit_vector(31 downto 0);
			o : out bit_vector(63 downto 0)
		);
	end component;

	component PC is
		generic(
			wordSize : natural := 64
		);
		port(
			clock: in bit;
			reset: in bit;
			load: in bit;
			d: in bit_vector(wordSize-1 downto 0);
			q: out bit_vector(wordSize-1 downto 0)
		);
	end component;

	component regfile is 
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
	end component;

	component shiftLeft2 is 
		generic (
			wordSize : natural := 64
		);
		port(
			i: in bit_vector(wordSize-1 downto 0);
			o: out bit_vector(wordSize-1 downto 0)
		);
	end component;

	component mux is
		generic(
			wordSize: natural := 64
		);
		port(
			sel: in bit;
			A: in bit_vector(wordSize -1 downto 0);
			B: in bit_vector(wordSize -1 downto 0);
			O: out bit_vector(wordSize -1 downto 0)
		);
	end component;

	signal pc_input: bit_vector(wordSize-1 downto 0);
	signal pc_output: bit_vector(wordSize-1 downto 0);
	signal four_vector: bit_vector(wordSize-1 downto 0);
	signal pc_adder_output: bit_vector(wordSize-1 downto 0);
	signal branch_alu_output: bit_vector(wordSize-1 downto 0);
	signal sl2_output: bit_vector(wordSize-1 downto 0);
	signal instruction: bit_vector(31 downto 0);
	signal extended_address: bit_vector(wordSize-1 downto 0);
	signal read_register1: bit_vector(4 downto 0);
	signal read_register2: bit_vector(4 downto 0);
	signal write_register: bit_vector(4 downto 0);
	signal write_data: bit_vector(wordSize-1 downto 0);
	signal reg_output1: bit_vector(wordSize-1 downto 0);
	signal reg_output2:	bit_vector(wordSize-1 downto 0);
	signal regmux_entry0: bit_vector(4 downto 0);
	signal regmux_entry1: bit_vector(4 downto 0);
	signal main_alu_entry1: bit_vector(wordSize-1 downto 0);
	signal main_alu_out: bit_vector(wordSize-1 downto 0);
	
	begin
		four_vector <= "0000000000000000000000000000000000000000000000000000000000000100";
		instruction <= imOut;
		regmux_entry0 <= instruction(20 downto 16);
		regmux_entry1 <= instruction(4 downto 0);
		opcode <= ("10110100" & "000") when instruction(31 downto 24) = "10110100" else
				  ("000101" & "00000") when instruction(31 downto 26) = "000101" else
				  instruction(31 downto 21);
		read_register1 <= instruction(9 downto 5);
		write_register <= instruction(4 downto 0);

		pcomponent : PC 
		generic map (64)
		port map(clock,reset,'1',pc_input,pc_output);

		alu1 : alu
		generic map (64)
		port map(pc_output,four_vector,pc_adder_output,"0010",open,open,open);

		alu2 : alu 
		generic map (64)
		port map(pc_output,sl2_output,branch_alu_output,"0010",open,open,open);

		alu3 : alu
		generic map(64)
		port map(reg_output1,main_alu_entry1,main_alu_out,aluCtrl,zero,open,open);

		mux1 : mux
		generic map(64)
		port map(aluSrc,reg_output2,extended_address,main_alu_entry1);

		mux2 : mux 
		generic map(64)
		port map(pcsrc,pc_adder_output,branch_alu_output,pc_input);

		mux3: mux
		generic map(5)
		port map(reg2loc,regmux_entry0,regmux_entry1,read_register2);

		mux4: mux
		generic map(64)
		port map(memToReg,main_alu_out,dmOut,write_data);

		registradores: regfile
		generic map (32,64)
		port map(clock,reset,regWrite,read_register1,read_register2,write_register,write_data,reg_output1,reg_output2);

		extensor: signExtend
		port map(instruction,extended_address);

		mult4: shiftLeft2
		generic map (64)
		port map(extended_address,sl2_output);

		dmAddr <= main_alu_out;
		dmIn <= reg_output2;
		imAddr <= pc_output;

end arquite;

library ieee;
use ieee.numeric_bit.ALL;
use ieee.math_real.ALL;

entity polilegsc is
	port (
		clock, reset: in bit;
		dmem_addr: out bit_vector(63 downto 0);
		dmem_dati: out bit_vector(63 downto 0);
		dmem_dato: in  bit_vector(63 downto 0);
		dmem_we:   out bit;
		imem_addr: out bit_vector(63 downto 0);
		imem_data: in  bit_vector(31 downto 0)
	);
end polilegsc;

architecture arquit of polilegsc is
	

	component datapath is
		generic(
			wordSize: natural := 64
		);
		port(
			clock: in bit;
			reset: in bit;
			reg2loc: in bit;
			pcsrc: in bit;
			memToReg: in bit;
			aluCtrl: in bit_vector(3 downto 0);
			aluSrc: in bit;
			regWrite: in bit;
			opcode: out bit_vector(10 downto 0);
			zero: out bit;
			imAddr: out bit_vector(63 downto 0);
			imOut: in bit_vector(31 downto 0);
			dmAddr: out bit_vector(63 downto 0);
			dmIn: out bit_vector(63 downto 0);
			dmOut: in bit_vector(63 downto 0)
		);
	end component;

	component alucontrol is
		port (
		  aluop:   in  bit_vector(1 downto 0);
		  opcode:  in  bit_vector(10 downto 0);
		  aluCtrl: out bit_vector(3 downto 0)
		);
	end component;
	
	component controlunit is
		port(
			reg2loc: out bit;
			uncondbranch: out bit;
			branch : out bit;
			memRead : out bit;
			memToReg: out bit;
			aluOp : out bit_vector(1 downto 0);
			memWrite: out bit;
			aluSrc : out bit;
			regWrite : out bit;
			opcode: in bit_vector(10 downto 0)
		);
	end component controlunit;

	signal opcode_s: bit_vector(10 downto 0);
	signal reg2loc_s: 	bit;
	signal uncondBranch_s: 	bit;
	signal branch_s: 	bit;
	signal memRead_s: 	bit;
	signal memToReg_s: 	bit;
	signal aluOp_s: 	bit_vector(1 downto 0);
	signal memWrite_s: 	bit;
	signal aluSrc_s: 	bit;
	signal regWrite_s: 	bit;
	signal aluCtrl_s: 	bit_vector(3 downto 0);
	signal pcsrc_s : 	bit;
	signal zero_s: 		bit;
	signal zero_branch: bit;

	begin
		dmem_we <= 	'0' when memRead_s = '1' else
					'1' when memWrite_s = '1';
		
		zero_branch <= branch_s and zero_s;
		pcsrc_s <= uncondBranch_s or zero_branch;
		
		datapathcomponent: datapath
		generic map(64)
      port map(clock, reset, reg2loc_s, pcsrc_s, memtoreg_s, aluctrl_s, alusrc_s, regwrite_s, opcode_s, zero_s, imem_addr, imem_data, dmem_addr, dmem_dati, dmem_dato);
					
		controlunitcomponent: controlunit
		port map(reg2loc_s, uncondBranch_s, branch_s, memRead_s, memToReg_s, aluOp_s, memWrite_s, aluSrc_s, regWrite_s, opcode_s);
		
		alucontrolcomponent: alucontrol
		port map(aluOp_s, opcode_s, aluctrl_s);
				
end arquit;
