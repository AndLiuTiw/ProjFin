// NOTE : Below mentioned frame work is for reference purpose.
module simplified_sha256 #(parameter integer NUM_OF_WORDS = 20)(
 input logic  clk, reset_n, start,
 input logic  [15:0] message_addr, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data);

// FSM state variables 
enum logic [2:0] {IDLE, READ, BLOCK, COMPUTE, WRITE} state;

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[64]; //To be used in word expansion and sha operation steps, have not initialized yet
logic [31:0] message[20]; //These are the 20 message blocks, each one is 32 bits
logic [31:0] wt; //Wtf is this (unused for now)
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7; //initialized in always_ff block BLOCK state
logic [31:0] a, b, c, d, e, f, g, h; //initialized in always_ff block IDLE state
logic [ 7:0] i, j, ind, ind2; //i has been initialized in IDLE, we're using j as the index variable to load different blocks, ind is being used in the WRITE state, ind2 is being usd in the READ state
logic [15:0] offset; // in word address //initialized in IDLE state
logic [ 7:0] num_blocks; //initialized by determine_num_blocks function
logic        cur_we; //initialized in IDLE state 
logic [15:0] cur_addr; //Initialized in IDLE state
logic [31:0] cur_write_data; //Unitialized, it's use is understood
logic [511:0] memory_block; //Not sure how this is to be used (unitialized)
logic [ 7:0] tstep; //initialized by starter code
//logic [255:0] sha256_func_output;
//logic [31:0] S1, S0, ch, maj, t1, t2; // added to bypass sha256_op function

////Logic added to debug red in message
//logic [31:0] m0;
//logic [31:0] m1;
//logic [31:0] m2;
//logic [31:0] m3;
//logic [31:0] m4;
//logic [31:0] m5;
//logic [31:0] m6;
//logic [31:0] m7;
//logic [31:0] m8;
//logic [31:0] m9;
//logic [31:0] m10;
//logic [31:0] m11;
//logic [31:0] m12;
//logic [31:0] m13;
//logic [31:0] m14;
//logic [31:0] m15;
//logic [31:0] m16;
//logic [31:0] m17;
//logic [31:0] m18;
//logic [31:0] m19;
//assign m0 = message[0];
//assign m1 = message[1];
//assign m2 = message[2];
//assign m3 = message[3];
//assign m4 = message[4];
//assign m5 = message[5];
//assign m6 = message[6];
//assign m7 = message[7];
//assign m8 = message[8];
//assign m9 = message[9];
//assign m10 = message[10];
//assign m11 = message[11];
//assign m12 = message[12];
//assign m13 = message[13];
//assign m14 = message[14];
//assign m15 = message[15];
//assign m16 = message[16];
//assign m17 = message[17];
//assign m18 = message[18];
//assign m19 = message[19];
////End of logic added to debug red in message

////Logic added to debug w
//logic [31:0] w0;
//logic [31:0] w1;
//logic [31:0] w2;
//logic [31:0] w3;
//logic [31:0] w4;
//logic [31:0] w5;
//logic [31:0] w6;
//logic [31:0] w7;
//logic [31:0] w8;
//logic [31:0] w9;
//logic [31:0] w10;
//logic [31:0] w11;
//logic [31:0] w12;
//logic [31:0] w13;
//logic [31:0] w14;
//logic [31:0] w15;
//assign w0 = w[0];
//assign w1 = w[1];
//assign w2 = w[2];
//assign w3 = w[3];
//assign w4 = w[4];
//assign w5 = w[5];
//assign w6 = w[6];
//assign w7 = w[7];
//assign w8 = w[8];
//assign w9 = w[9];
//assign w10 = w[10];
//assign w11 = w[11];
//assign w12 = w[12];
//assign w13 = w[13];
//assign w14 = w[14];
//assign w15 = w[15];
////End of logic added to debug w

//logic [31:0] debug;
//assign debug = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);

// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


assign num_blocks = determine_num_blocks(NUM_OF_WORDS); 
assign tstep = (i - 8'd1);

// Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
// Function to determine number of blocks in memory to fetch
function logic [7:0] determine_num_blocks(input logic [31:0] size);

  // Student to add function implementation
  //According to the part1 document, since num of words is hardcoded to 20, there will be 2 blocks only
  determine_num_blocks = 8'd2;
endfunction


// SHA256 hash round
//function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
//                                 input logic [7:0] t);
//    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
//	 logic [255:0] packed_return; //return value
//begin
//    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
//    // Student to add remaning code below
//    // Refer to SHA256 discussion slides to get logic for this function
//    ch = (e & f) ^ ((~e) & g);
//    t1 = h + S1 + ch + k[tstep] + w[tstep];
//    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
//    maj = (a & b) ^ (a & c) ^ (b & c);
//    t2 = S0 + maj;
//    packed_return = {t1 + t2, a, b, c, d + t1, e, f, g};
//	 return packed_return;
//end
//endfunction


// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign mem_clk = clk;
assign mem_addr = cur_addr + offset; 
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;


// Right Rotation Example : right rotate input x by r
// Lets say input x = 1111 ffff 2222 3333 4444 6666 7777 8888
// lets say r = 4
// x >> r  will result in : 0000 1111 ffff 2222 3333 4444 6666 7777 
// x << (32-r) will result in : 8888 0000 0000 0000 0000 0000 0000 0000
// final right rotate expression is = (x >> r) | (x << (32-r));
// (0000 1111 ffff 2222 3333 4444 6666 7777) | (8888 0000 0000 0000 0000 0000 0000 0000)
// final value after right rotate = 8888 1111 ffff 2222 3333 4444 6666 7777
// Right rotation function
function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
   rightrotate = (x >> r) | (x << (32 - r));
endfunction


// SHA-256 FSM 
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory
always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    cur_we <= 1'b0;
    state <= IDLE;
  end 
  else case (state)
    // Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
    IDLE: begin 
       if(start) begin
       // Student to add rest of the code
		   //a through h are initialized to initial hash values
			a <= 32'h6a09e667; 
			b <= 32'hbb67ae85;
			c <= 32'h3c6ef372;
			d <= 32'ha54ff53a;
			e <= 32'h510e527f;
			f <= 32'h9b05688c;
			g <= 32'h1f83d9ab;
			h <= 32'h5be0cd19;
			cur_we <= 0; //Because nothing needs to be written to memory right now (in the idle state)
			offset <= 0; //Should probably be 0 initially
			cur_addr <= message_addr; //Because curr_addr should be initialized to 1st message location (address of W0 (We have words from W0 to W15))in memory
			i <= 1; //Initializing to 1 because tstep = i - 1 and tstep should start from 0
			j <= 0; //Don't even know if this will be used
			ind2 <= 0; //Because next state is the READ state
			offset <= 0; //Because next state is READ state and I need to create a 1 cycle gap
			state <= READ;
       end
    end
	 
	 //Adding a READ state to Read 640 bits message from testbench memory in chunks of 32bits words (i.e. read 20 locations from memory by incrementing address offset)
	 READ: begin
		if(ind2 > 0) begin
			message[ind2 - 1] <= mem_read_data;
		end
		if(ind2 <= 19) begin
			offset <= offset + 16'd1; //To read from next address in memory
			ind2 <= ind2 + 8'd1;
			state <= READ;
		end
		else begin
			state <= BLOCK;
		end
	 end

    // SHA-256 FSM 
    // Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function    
    // and write back hash value back to memory
    BLOCK: begin
	// Fetch message in 512-bit block size
	// For each of 512-bit block initiate hash value computation
	   h0 <= a;
		h1 <= b;
		h2 <= c;
		h3 <= d;
		h4 <= e;
		h5 <= f;
		h6 <= g;
		h7 <= h;
		if (j == 0) begin 
			memory_block <= {message[15],message[14],message[13],message[12],message[11],message[10],message[9],message[8],message[7],message[6],message[5],message[4],message[3],message[2],message[1],message[0]};
			j <= j + 8'd1;
			state <= COMPUTE;
			i <= 1;
		end
		else if (j == 1) begin
			memory_block <= {64'd640,319'b0,1'b1,message[19],message[18],message[17],message[16]};
			j <= j + 8'd1;
			state <= COMPUTE;
			i <= 1;
		end
		else begin //j is equal to 2
			cur_we <= 1; //Because the next state is the WRITE state
			cur_addr <= output_addr; //Because the next state is the WRITE state
			offset <= 0; //Because the next state is the WRITE state
			ind <= 0; //Because the next state is the WRITE state
			cur_write_data <= h0; //Will be written at next clock edge
			state <= WRITE;
		end
    end

    // For each block compute hash function
    // Go back to BLOCK stage after each block hash computation is completed and if
    // there are still number of message blocks available in memory otherwise
    // move to WRITE stage
    COMPUTE: begin
	// 64 processing rounds steps for 512-bit block 
        if (i <= 64) begin //For i values from 1 to 64, this is the word expansion part
			case (i)
				1 : begin
					w[tstep] <= memory_block[31:0];
				end
				2 : begin
					w[tstep] <= memory_block[63:32];
				end
				3 : begin
					w[tstep] <= memory_block[95:64];
				end
				4 : begin
					w[tstep] <= memory_block[127:96];
				end
				5 : begin
					w[tstep] <= memory_block[159:128];
				end
				6 : begin
					w[tstep] <= memory_block[191:160];
				end
				7 : begin
					w[tstep] <= memory_block[223:192];
				end
				8 : begin
					w[tstep] <= memory_block[255:224];
				end
				9 : begin
					w[tstep] <= memory_block[287:256];
				end
				10 : begin
					w[tstep] <= memory_block[319:288];
				end
				11 : begin
					w[tstep] <= memory_block[351:320];
				end
				12 : begin
					w[tstep] <= memory_block[383:352];
				end
				13 : begin
					w[tstep] <= memory_block[415:384];
				end
				14 : begin
					w[tstep] <= memory_block[447:416];
				end
				15 : begin
					w[tstep] <= memory_block[479:448];
				end
				16 : begin
					w[tstep] <= memory_block[511:480];
				end
				default : begin //For i = 17 to 64
					w[tstep] <= w[tstep - 16] + w[tstep - 7] + (rightrotate(w[tstep - 15],7) ^ rightrotate(w[tstep - 15],18) ^ (w[tstep - 15] >> 3)) + (rightrotate(w[tstep - 2],17) ^ rightrotate(w[tstep - 2],19) ^ (w[tstep - 2] >> 10));
				end
			endcase
			i <= i + 8'd1;
			state <= COMPUTE; //Go back to compute state if i is less than or equal to 64
		  end
		  else if(i <= 128) begin //For i values from 65 to 128. this is the sha256_op part
//		   //Start of Section added to bypass sha256_op function
//		   S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
//			ch = (e & f) ^ ((~e) & g);
//			t1 = h + S1 + ch + k[tstep] + w[tstep];
//			S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
//			maj = (a & b) ^ (a & c) ^ (b & c);
//			t2 = S0 + maj;
//			//End of Section added to bypass sha256_op function
			a <= h + (rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25)) + ((e & f) ^ ((~e) & g)) + k[tstep - 8'd64] + w[tstep - 8'd64] + (rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22)) + ((a & b) ^ (a & c) ^ (b & c));
			b <= a;
			c <= b;
			d <= c;
			e <= d + h + (rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25)) + ((e & f) ^ ((~e) & g)) + k[tstep - 8'd64] + w[tstep - 8'd64];
			f <= e;
			g <= f;
			h <= g;
			i <= i + 8'd1;
			state <= COMPUTE; //Go back to compute if i value is in [65, 128]
		  end
		  else begin //For i value 29
		   //a through h are going to be used again BLOCK state to initialize h0 to h7
		   a <= a + h0;
			b <= b + h1;
			c <= c + h2;
			d <= d + h3;
			e <= e + h4;
			f <= f + h5;
			g <= g + h6;
			h <= h + h7;
			state <= BLOCK; //Go to BLOCK state if i value is 129
		  end
    end

    // h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
    // h0 to h7 after compute stage has final computed hash value
    // write back these h0 to h7 to memory starting from output_addr
    WRITE: begin
		case (ind)
			0 : begin
				offset <= 1;
				cur_write_data <= h1;
				ind <= ind + 8'd1;
				state <= WRITE;
			end
			1 : begin
				offset <= 2;
				cur_write_data <= h2;
				ind <= ind + 8'd1;
				state <= WRITE;
			end
			2 : begin
				offset <= 3;
				cur_write_data <= h3;
				ind <= ind + 8'd1;
				state <= WRITE;
			end
			3 : begin
				offset <= 4;
				cur_write_data <= h4;
				ind <= ind + 8'd1;
				state <= WRITE;
			end
			4 : begin
				offset <= 5;
				cur_write_data <= h5;
				ind <= ind + 8'd1;
				state <= WRITE;
			end
			5 : begin
				offset <= 6;
				cur_write_data <= h6;
				ind <= ind + 8'd1;
				state <= WRITE;
			end
			6 : begin
				offset <= 7;
				cur_write_data <= h7;
				ind <= ind + 8'd1;
				state <= WRITE;
			end
			7 : begin
				offset <= 0;
				cur_we <= 0; //Because next state is IDLE state
				ind <= 8'd0;
				state <= IDLE;
			end
		endcase
    end
   endcase
  end

//assign sha256_func_output = sha256_op(a, b, c, d, e, f, g, h, w, tstep);
// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule
