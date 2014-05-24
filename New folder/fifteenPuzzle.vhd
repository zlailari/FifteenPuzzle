------------------------------------------------------------------------------
-- Fifteen Puzzle game
-- your name
--
-- This circuit implements the well-known 15 puzzle on the prototype board.
-- 
-- your documentation here
--
-- When a game is initialized, we place all the tiles in order and then scramble
-- them 32 times.  We use the rand function with a constant seed to scramble the board.
-- The game ends when all the tiles are in order.  We check this by matching the square
-- value with the value of the index in our board. The emptySquare is given the value 0.
--
-- Input Signals:
-- nuPuzzle is a one bit input signal that tells the circuit whether or not
-- to create a new puzzle.
-- moveDir is a two bit input signal that specifies which square adjacent to
-- the empty square will be moved: 0 => top; 1 => right; 2 => bottom; 3 => left
-- moveNow is a one bit signal vector that specifies when a move should be 
-- performed.
-- peekPos specifies a board position
--
-- Output Signals:
-- peekVal is the value at PeekPos in the gameBoard
-- emptyPos is the position of the empty square
-- puzlTime is the amount of time that the user has been playing the current game
-- bestTime is the fastest time the user has completed the game.
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use work.commonDefs.all;

entity fifteenPuzzle is port (
   clk, reset: in std_logic;
   -- client-side interface
   nuPuzzle: in std_logic;
   moveDir: in std_logic_vector(1 downto 0);
   moveNow: in std_logic;
   puzlTime, bestTime: out bcdVec(5 downto 0);
   -- auxiliary signals for testing
   peekPos: in nibble; -- specifies a board position
   peekVal: out nibble; -- tile at position peekPos
   emptyPos: out nibble; -- position of empty square
   -- interface to external display
   hSync, vSync: out std_logic;
   dispVal: out pixel);
end fifteenPuzzle;

architecture a1 of fifteenPuzzle is

component copyPattern port( 
   clk, reset : in  std_logic;
   -- client-side interface
   start, highlight: in std_logic;
   pattern: in nibble;
   position: in nibble;
   busy: out std_logic;
   -- interface to external display
   hSync, vSync: out std_logic;
   dispVal: out pixel);
end component;

-- state of puzzle controller
type stateType is (init, scramble, solveIt, puzzleSolved);
signal state: stateType;

-- array of nibbles defines the state of the puzzle
type boardArray is array(0 to 15) of nibble;
signal board: boardArray;

signal emptySquare: nibble; -- position of the empty space
signal randBits: word; -- random bits for scrambling puzzle

-- signals to control copyPattern
signal timer: std_logic_vector(10+operationMode*5 downto 0);
signal startCopy, busy, highlight: std_logic;
signal pos, nbor, pat: nibble;

--Added signal count for updating puzlTime
signal count: std_logic_vector(19 downto 0);
signal puzler: bcdVec(5 downto 0);
signal bTime: bcdVec(5 downto 0);

--Added signals for movement
signal dirI: nibble;
signal emptyI: nibble;

--Added signals count
signal cnt: std_logic_vector(4 downto 0);

-- TODO - add other signals you may need

-- Return next pseudo-random value following r
function random(r: word) return word is begin
   return (r(5) xor r(3) xor r(2) xor r(0)) & r(15 downto 1);
end;

-- Return true if emptySquare is in top row, else false.
impure function topRow return boolean is begin
   return emptySquare(3 downto 2) = "00";
end;
-- Return true if emptySquare is in bottom row, else false.
impure function botRow return boolean is begin
   --nibble is (3 downto 0)
   return emptySquare(3 downto 2) = "11";
end;
-- Return true if emptySquare is in leftmost column row, else false.
impure function leftCol return boolean is begin
   return emptySquare(1 downto 0) = "00";
end;
-- Return true if emptySquare is in rightmost column, else false.
impure function rightCol return boolean is begin
   return emptySquare(1 downto 0) = "11";
end;

-- Return true if the current board state matches the "goal" state.
-- That is, tile number i is at position i for all values of i.
impure function solved return boolean is begin
   for I in 0 to 15 loop
      if unsigned(board(I)) /= I then
         return false;
      end if;
   end loop;
   return true;

end;
begin
   -- process to setup puzzle and respond to player's moves
   process(clk) 
   -- Move the empty square in the direction specified by dir.
   -- Update both the game board and the emptySquare signal.
   -- Dir=0 means move up, 1 means move right, 2 means move down
   -- and 3 means move left.
   -- Ignore requests that would move the empty square off the
   -- board (so if you're in the top row, ignore up-moves).
   procedure move(dir: std_logic_vector(1 downto 0)) is begin
      --Move empty Square to Top
      if unsigned(dir) = 0 and not topRow then
         board(int(emptySquare)) <= board(int(emptySquare)-4);
         board(int(emptySquare)-4) <= x"0";
			emptySquare <= emptySquare - 4;
		--Move empty square to right
      elsif unsigned(dir) = 1 and not rightCol then
         board(int(emptySquare)) <= board(int(emptySquare)+1);
         board(int(emptySquare)+1) <= x"0";
			emptySquare <= emptySquare + 1;
		--Move empty square to bottom
      elsif unsigned(dir) = 2 and not botRow then
         board(int(emptySquare)) <= board(int(emptySquare)+4);
         board(int(emptySquare)+4) <= x"0";
			emptySquare <= emptySquare + 4;
		--Move empty Square to left
      elsif unsigned(dir) = 3 and not leftCol then
         board(int(emptySquare)) <= board(int(emptySquare)-1);
         board(int(emptySquare)-1) <= x"0";
			emptySquare <= emptySquare - 1;
      end if;
   end;
	
   begin
      if rising_edge(clk) then
         if reset = '1' then
            randBits <= x"357d"; -- initial random value
            count <= (others => '0'); cnt <= (others => '0');
            emptySquare <= x"0";    
				state <= init;
				puzler <= (others=> x"0");
				bTime <= (others=> x"9");
         elsif nuPuzzle = '1' then
				puzler <= (others=> x"0");
            state <= scramble;
            count <= (others => '0'); cnt <= (others => '0');
         else
            case state is
					when init => 
						if unsigned(cnt) > 15 then
							cnt<= (others => '0');
							state <= scramble;
						else
							board(int(cnt)) <= cnt(3 downto 0);
							cnt <= cnt + 1;
						end if;
						
					when scramble =>
						randBits <= random(randBits);
						
						--Chooses random move base on randBits value
						move(randBits(1 downto 0));
							
						count <= count + 1;
						
						--Specifies number of scramble moves based on operationMode
						if operationMode = 0 then
							if count > 30 then
								state <= solveIt;
							end if;
						elsif operationMode = 1 then
							if count > 5 then
								state <= solveIt;
							end if;
						end if;
						-- scramble the tiles on the board
						-- by making random moves
						-- do 32 moves when operationMode=0
						-- do 1024 when operationMode=1
						-- when done, go to solveIt state
						-- and start puzlTime counting         
					when solveIt =>
						if solved then
							--stop counting puzlTime, update bestTime
							state <= puzzleSolved;
						else 
							if moveNow = '1' then
								move(moveDir);
							end if;
							
							count <= count + 1;
							
							if unsigned(count) = 50000 then
								puzler <= plus1(puzler);
								count <= (others => '0');
							end if;
						end if;
						
						-- TODO
						-- if in the goal state
						--   stop counting puzlTime and
						--     update bestTime;
						--   go to solved state;
						-- otherwise, if a move is requested
						--   them make the move;
						-- increment puzlTime every 10 ms
					
					when puzzleSolved =>
						if lessThan(puzler, bTime) then
							bTime <= puzler;
						end if;
					when others =>
            end case;
         end if;
      end if;

      
   end process;
   bestTime <= bTime;
   puzlTime <= puzler;            
   -- Process to control copying of patterns to display.
   -- Increment timer to control start of copying operation.
   -- Increment pos for each new copy operation, in order to
   -- iterate through all positions on the board.
   process(clk) begin
      if rising_edge(clk) then
         if reset = '1' then
            timer <= (others => '0');
            pos <= (others => '0');
            -- TODO
         else
            if timer = (timer'range =>'0') then
					pos <= pos + 1;
				end if;
				timer <= timer + 1;
            -- TODO
         end if;
      end if;
   end process;

   -- copy pattern for tile at position pos to its place on the display
   cp: copyPattern port map(clk,reset,startCopy,highlight,pat,
                            pos,busy,hSync,vSync,dispVal);
   startCopy <= '1' when timer = (timer'range => '0') else '0';
   
   pat <= board(int(pos));
	-- TODO pattern to be displayed in the current position
   
   -- highlight the neighbor of the empty square, selected by moveDir
   highlight <= '1' when nbor = pos and (nbor /= emptySquare)
                else '0';
   
   nbor <=  (emptySquare - 4) when moveDir = "00" and not topRow else
            (emptySquare + 1) when moveDir = "01" and not rightCol else
            (emptySquare + 4) when moveDir = "10" and not botRow else
            (emptySquare - 1) when moveDir = "11" and not leftCol else 
            emptySquare;
            
            -- TODO - nbor=position of the neighbor of emptySquare
           -- selected by moveDir;
           -- if the neighbor is off the board, make nbor=emptySquare
                
   -- auxiliary signals
   emptyPos <= emptySquare;
   peekVal <= board(int(peekPos));
end a1;
