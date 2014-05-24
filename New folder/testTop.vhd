--------------------------------------------------------------------------------
-- Testbench for fifteenPuzzle on prototype board
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use work.txt_util.all;
use work.commonDefs.all;
 
ENTITY testTop IS
END testTop;
 
architecture arch of testTop is
component top port(
   clk: in std_logic;
   -- S3 board buttons, knob, switches and LEDs
   btn: in buttons;
   knob: in knobSigs;
   swt: in switches;
   lcd: out lcdSigs;
   led: out leds;
   -- signals for controlling VGA display 
   hSync, vSync: out std_logic;
   dispVal: out pixel);
end component;
    
--Inputs
signal clk : std_logic := '0';
signal btn : std_logic_vector(3 downto 0) := (others => '0');
signal knob : std_logic_vector(2 downto 0) := (others => '0');
signal swt : std_logic_vector(3 downto 0) := (others => '0');

--Outputs
signal led : std_logic_vector(7 downto 0);
signal lcd : lcdSigs;
signal hSync, vSync: std_logic;
signal dispVal: pixel;

-- Clock period definitions
constant clk_period : time := 20 ns;
constant pause : time := 5*clk_period;

signal press: std_logic := '0';
signal rot: std_logic_vector(1 downto 0) := "00";

signal moveDir: integer := 0;
 
begin
 
-- Instantiate the Unit Under Test (UUT)
   uut: top port map (clk,btn,knob,swt,lcd,led,hSync,vSync,dispVal);

   -- Clock process definitions
   process begin
      clk <= '0'; wait for clk_period/2;
      clk <= '1'; wait for clk_period/2;
   end process;
   
   knob(2 downto 1) <= rot;
   knob(0) <= press;

   -- Stimulus process
   process 
   -- rotate the knob to the right cnt times
   procedure rrot(cnt: in integer) is
   begin
      for i in 1 to cnt loop
         rot <= "10"; wait for pause; rot <= "11"; wait for pause;
         rot <= "01"; wait for pause; rot <= "00"; wait for pause;
      end loop;
   end;
   
   -- rotate the knob to the left cnt times
   procedure lrot(cnt: in integer) is
   begin
      for i in 1 to cnt loop
         rot <= "01"; wait for pause; rot <= "11"; wait for pause;
         rot <= "10"; wait for pause; rot <= "00"; wait for pause;
      end loop;
   end;
   
   -- push the knob button cnt times
   procedure bump(cnt: in integer) is
   begin
      for i in 1 to cnt loop
         press <= '1'; wait for pause; press <= '0'; wait for pause;
      end loop;
   end;
   
   -- push the reset button
   procedure reset is
   begin
      btn(0) <= '1'; wait for pause; btn(0) <= '0'; wait for pause;
   end;
   
   -- Procedure to start a new puzzle
	procedure nuPuzzle is
	begin
		btn(1) <= '1'; wait for pause; btn(1) <= '1'; wait for pause;
	end;
		-- Procedure to check that the tile at position pos is the one
		-- specified by val
	procedure checkIt(pos: integer; val: integer) is
	begin
		swt <= slv(pos,4); wait for 1 ps; 
		assert led(3 downto 0) = slv(val,4) report "value not consistent"& str(pos) &" should be "& str(val);
	end;
		
		-- Move the empty square in the direction indicated by dir.
		-- Check that there is a zero at the position indicated by the
		-- emptyPos output.
	procedure move(dir: integer) is -- TODO
	begin
		if dir > moveDir then rrot(dir - moveDir);
		elsif dir < movedir then lrot(moveDir-dir);
		end if;
		moveDir <= dir;
		btn(2) <= '1'; wait for pause; 
		btn(2) <='0'; wait for pause;
		checkIt(int(led(7 downto 4)),0);
	end;
		
	begin      
      reset; wait for 10 us;

      -- check that the board is in the right configuration
		checkIt(0,1); checkIt(1,5); checkIt(2,2); checkIt(3,3);
		checkIt(4,4); checkIt(5,9); checkIt(6,0); checkIt(7,7);
		checkIt(8,8); checkIt(9,10); checkIt(10,6); checkIt(11,14);
		checkIt(12,12); checkIt(13,13); checkIt(14,15); checkIt(15,11);

		-- make moves to solve the puzzle, but stop before the last move
		move(2); move(1); move(2); move(3); move(0);
		move(3); move(0); move(0); --move(3);

      wait for 10 ms;
      -- make the last move
		move(3);
      -- TODO - check that the game is in the goal position
		for i in 0 to 15 loop
				checkIt(i,i);
		end loop;
      
      nuPuzzle; wait for 10 us;

      -- make a few moves
		move(1); move(3); move(2); move(1); move(1); move(0);

      wait for 20 ms;

      assert false report "simulation ended normally" severity failure;
   end process;

END;