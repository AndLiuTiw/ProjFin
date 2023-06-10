module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;

// FSM state variables 
enum logic [2:0] {IDLE, READ, BLOCK, COMPUTE, WRITE} state;
//Phase variable (to indicate which phase of computation is underway)
enum logic [1:0] {ONE, TWO, THREE, DONE} phase;
//Compute_sub states to increase speed
logic [6:0] compute_sub_state;

logic [6:0] sha_idx;

//logic [ 4:0] state; //Was given in starter code, commented out by me
logic [31:0] hout[num_nonces];

//Instead of h0 to h7 like in part 1, I am using H[i] as hash value in part 2
logic [31:0] H[16][8]; //16 because one for each nonce, 8 because H0 to H7 requird for each computation, only H[0] is used for phase one 
logic [31:0] a[16], b[16], c[16], d[16], e[16], f[16], g[16], h[16]; //One for each nonce, only a[0], b[0], ..., h[0] is used in phase 1

logic [31:0] message[19]; //These are the 19 message blocks (not 20 because 20th block is a nonce), each one is 32 bits, 1 required for each nonce, message[0] i sused for phase one

//Array because 1 required for each nonce for one or more phases
logic [31:0] w[16][64]; //16 because one for each nonce, 64 because 64 words are required in word expansion, only w[0] is used for phase one

logic [5:0] read_idx; //Index used to read values from memory, goes from 0 to 19;

logic [3:0] write_idx; //Index used to write values to memory in WRITE state. goes from 0 to 15;

logic [511:0] memory_blocks[16]; //1 required for each nonce value (only memory_blocks[0] is used for phase 1)

parameter int k[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

assign mem_clk = clk; //Both clocks are concurrent
assign done = (phase == DONE); //Computation is complete when phase is DONE

//rightrotate function definition
function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
   rightrotate = (x >> r) | (x << (32 - r));
endfunction

// Student to add rest of the code here
always_ff @(posedge clk, negedge reset_n)
begin
	if (!reset_n) begin
    mem_we <= 1'b0; //Because we don't want to write anything in the beginning 
    state <= IDLE; //The first state to go to
	 phase <= ONE; //Always start at phase 1
	 mem_addr <= message_addr; //Because we will be reading first 
	 read_idx <= 0; //0 is obviously the reset value
  end 
  else case (state)
	IDLE: begin //IDLE only occurs in phase 1, so no need to check for phase
		if (start) begin
			//initialized to initial hash values
			H[0][0] <= 32'h6a09e667; 
			H[0][1] <= 32'hbb67ae85;
			H[0][2] <= 32'h3c6ef372;
			H[0][3] <= 32'ha54ff53a;
			H[0][4] <= 32'h510e527f;
			H[0][5] <= 32'h9b05688c;
			H[0][6] <= 32'h1f83d9ab;
			H[0][7] <= 32'h5be0cd19;
			state <= READ;
			mem_we <= 1'b0; //Nothing to write when going to next state
			mem_addr <= message_addr; //Because we will be reading first
			read_idx <= 0; //Because we need to fill from w0 to w15
		end
	end
	READ: begin //READ is performed only once (in phase 1), so no need to check phase
		if(read_idx > 0) begin
			message[read_idx - 1] <= mem_read_data;
		end
		if(read_idx <= 18) begin
			mem_addr <= mem_addr + 16'd1; //To read from next address in memory
			read_idx <= read_idx + 6'd1;
			state <= READ;
		end
		else begin
			read_idx <= 0; //Reset it for the future
			state <= BLOCK;
		end
	end
	BLOCK: begin //BLOCK only occurs in phase 1 and 2
		if(phase == ONE) begin
			memory_blocks[0] <= {message[15],message[14],message[13],message[12],message[11],message[10],message[9],message[8],message[7],message[6],message[5],message[4],message[3],message[2],message[1],message[0]};
			state <= COMPUTE;
			compute_sub_state <= 0; //In preparation for COMPUTE state 
		end
		else if(phase == TWO) begin
			//All 16 nonce memory_blocks are instantiated parallely
			memory_blocks[0] <= {32'd640,320'h00000000,32'h80000000,32'd0,message[18],message[17],message[16]};
			memory_blocks[1] <= {32'd640,320'h00000000,32'h80000000,32'd1,message[18],message[17],message[16]};
			memory_blocks[2] <= {32'd640,320'h00000000,32'h80000000,32'd2,message[18],message[17],message[16]};
			memory_blocks[3] <= {32'd640,320'h00000000,32'h80000000,32'd3,message[18],message[17],message[16]};
			memory_blocks[4] <= {32'd640,320'h00000000,32'h80000000,32'd4,message[18],message[17],message[16]};
			memory_blocks[5] <= {32'd640,320'h00000000,32'h80000000,32'd5,message[18],message[17],message[16]};
			memory_blocks[6] <= {32'd640,320'h00000000,32'h80000000,32'd6,message[18],message[17],message[16]};
			memory_blocks[7] <= {32'd640,320'h00000000,32'h80000000,32'd7,message[18],message[17],message[16]};
			memory_blocks[8] <= {32'd640,320'h00000000,32'h80000000,32'd8,message[18],message[17],message[16]};
			memory_blocks[9] <= {32'd640,320'h00000000,32'h80000000,32'd9,message[18],message[17],message[16]};
			memory_blocks[10] <= {32'd640,320'h00000000,32'h80000000,32'd10,message[18],message[17],message[16]};
			memory_blocks[11] <= {32'd640,320'h00000000,32'h80000000,32'd11,message[18],message[17],message[16]};
			memory_blocks[12] <= {32'd640,320'h00000000,32'h80000000,32'd12,message[18],message[17],message[16]};
			memory_blocks[13] <= {32'd640,320'h00000000,32'h80000000,32'd13,message[18],message[17],message[16]};
			memory_blocks[14] <= {32'd640,320'h00000000,32'h80000000,32'd14,message[18],message[17],message[16]};
			memory_blocks[15] <= {32'd640,320'h00000000,32'h80000000,32'd15,message[18],message[17],message[16]};
			state <= COMPUTE;
			compute_sub_state <= 0; //In preparation for COMPUTE state
		end
	end
	COMPUTE: begin //COMPUTE occurs in phase 1, 2 and 3
		if(phase == ONE) begin
//			if(compute_sub_state < 26) begin //Because the last two words, w[0][62] and w[0][63] are calculated when compute_state = 24 and compute_state = 25 is used to initialize values of a through h
//				case (compute_sub_state)
//					0: begin
//						//All of these can be set in parallel
//						w[0][0] <= memory_blocks[0][31:0];
//						w[0][1] <= memory_blocks[0][63:32];
//						w[0][2] <= memory_blocks[0][95:64];
//						w[0][3] <= memory_blocks[0][127:96];
//						w[0][4] <= memory_blocks[0][159:128];
//						w[0][5] <= memory_blocks[0][191:160];
//						w[0][6] <= memory_blocks[0][223:192];
//						w[0][7] <= memory_blocks[0][255:224];
//						w[0][8] <= memory_blocks[0][287:256];
//						w[0][9] <= memory_blocks[0][319:288];
//						w[0][10] <= memory_blocks[0][351:320];
//						w[0][11] <= memory_blocks[0][383:352];
//						w[0][12] <= memory_blocks[0][415:384];
//						w[0][13] <= memory_blocks[0][447:416];
//						w[0][14] <= memory_blocks[0][479:448];
//						w[0][15] <= memory_blocks[0][511:480];
//						compute_sub_state <= compute_sub_state + 7'd1;
//						state <= COMPUTE;
//					end
//					25: begin //Used to initialize a through h so that they can be used in sha256 operation step
//						a[0] <= H[0][0];
//						b[0] <= H[0][1];
//						c[0] <= H[0][2];
//						d[0] <= H[0][3];
//						e[0] <= H[0][4];
//						f[0] <= H[0][5];
//						g[0] <= H[0][6];
//						h[0] <= H[0][7];
//						state <= COMPUTE;
//						compute_sub_state <= compute_sub_state + 7'd1;
//					end
//					default: begin //two word computations can be parallelized because w[idx] is required to be known for computation of w[idx + 2]
//						w[0][2*compute_sub_state + 7'd14] <= w[0][2*compute_sub_state - 7'd2] + w[0][2*compute_sub_state + 7'd7] + (rightrotate(w[0][2*compute_sub_state - 7'd1],7) ^ rightrotate(w[0][2*compute_sub_state - 7'd1],18) ^ (w[0][2*compute_sub_state - 7'd1] >> 3)) + (rightrotate(w[0][2*compute_sub_state + 7'd12],17) ^ rightrotate(w[0][2*compute_sub_state + 7'd12],19) ^ (w[0][2*compute_sub_state + 7'd12] >> 10));
//						w[0][2*compute_sub_state + 7'd15] <= w[0][2*compute_sub_state - 7'd1] + w[0][2*compute_sub_state + 7'd8] + (rightrotate(w[0][2*compute_sub_state],7) ^ rightrotate(w[0][2*compute_sub_state],18) ^ (w[0][2*compute_sub_state] >> 3)) + (rightrotate(w[0][2*compute_sub_state + 7'd13],17) ^ rightrotate(w[0][2*compute_sub_state + 7'd13],19) ^ (w[0][2*compute_sub_state + 7'd13] >> 10));
//						compute_sub_state <= compute_sub_state + 7'd1;
//						state <= COMPUTE;
//					end
//				endcase
//			end
//			else if(compute_sub_state < 90) begin //For compute_sub_state >= 26 and <= 89, we peform sha256_op 64 times
//				a[0] <= h[0] + (rightrotate(e[0], 6) ^ rightrotate(e[0], 11) ^ rightrotate(e[0], 25)) + ((e[0] & f[0]) ^ ((~(e[0])) & g[0])) + k[compute_sub_state - 8'd26] + w[0][compute_sub_state - 8'd26] + (rightrotate(a[0], 2) ^ rightrotate(a[0], 13) ^ rightrotate(a[0], 22)) + ((a[0] & b[0]) ^ (a[0] & c[0]) ^ (b[0] & c[0]));
//				b[0] <= a[0];
//				c[0] <= b[0];
//				d[0] <= c[0];
//				e[0] <= d[0] + h[0] + (rightrotate(e[0], 6) ^ rightrotate(e[0], 11) ^ rightrotate(e[0], 25)) + ((e[0] & f[0]) ^ ((~(e[0])) & g[0])) + k[compute_sub_state - 8'd26] + w[0][compute_sub_state - 8'd26];
//				f[0] <= e[0];
//				g[0] <= f[0];
//				h[0] <= g[0];
//				compute_sub_state <= compute_sub_state + 7'd1;
//				state <= COMPUTE; //Go back to compute if compute_sub_state value is in [26, 89]
//			end
//			else begin //For compute_sub_state = 90, h0 through h7 (FOR EACH OF 16 NONCES) is set to the final result of this phase's computation
//				compute_sub_state <= 0; //reset for the future
//				for(int idx = 0; idx < 16; idx++) begin //This for loop sets h0 through h7 for EACH of 16 nonce values (they are all same and are equal to the oytput hash of phase one)
//					H[idx][0] <= H[0][0] + a[0];
//					H[idx][1] <= H[0][1] + b[0];
//					H[idx][2] <= H[0][2] + c[0];
//					H[idx][3] <= H[0][3] + d[0];
//					H[idx][4] <= H[0][4] + e[0];
//					H[idx][5] <= H[0][5] + f[0];
//					H[idx][6] <= H[0][6] + g[0];
//					H[idx][7] <= H[0][7] + h[0];
//				end
//				state <= BLOCK; //Phase 2 starts from BLOCK stage
//				phase <= TWO; //Moving to phase two now
//			end
			case (compute_sub_state)
				0: begin
					w[0][0] <= memory_blocks[0][31:0];
					w[0][1] <= memory_blocks[0][63:32];
					w[0][2] <= memory_blocks[0][95:64];
					w[0][3] <= memory_blocks[0][127:96];
					w[0][4] <= memory_blocks[0][159:128];
					w[0][5] <= memory_blocks[0][191:160];
					w[0][6] <= memory_blocks[0][223:192];
					w[0][7] <= memory_blocks[0][255:224];
					w[0][8] <= memory_blocks[0][287:256];
					w[0][9] <= memory_blocks[0][319:288];
					w[0][10] <= memory_blocks[0][351:320];
					w[0][11] <= memory_blocks[0][383:352];
					w[0][12] <= memory_blocks[0][415:384];
					w[0][13] <= memory_blocks[0][447:416];
					w[0][14] <= memory_blocks[0][479:448];
					w[0][15] <= memory_blocks[0][511:480];
					a[0] <= H[0][0];
					b[0] <= H[0][1];
					c[0] <= H[0][2];
					d[0] <= H[0][3];
					e[0] <= H[0][4];
					f[0] <= H[0][5];
					g[0] <= H[0][6];
					h[0] <= H[0][7];
					state <= COMPUTE;
					compute_sub_state <= compute_sub_state + 7'd1;
					sha_idx <= 0;
				end
				1: begin
					if(sha_idx == 64) begin
						sha_idx <= 0;
						compute_sub_state <= compute_sub_state + 1;
						state <= COMPUTE;
					end
					else begin 
						a[0] <= h[0] + (rightrotate(e[0], 6) ^ rightrotate(e[0], 11) ^ rightrotate(e[0], 25)) + ((e[0] & f[0]) ^ ((~e[0]) & g[0])) + k[sha_idx % 16] + w[0][sha_idx % 16] + (rightrotate(a[0], 2) ^ rightrotate(a[0], 13) ^ rightrotate(a[0], 22)) + ((a[0] & b[0]) ^ (a[0] & c[0]) ^ (b[0] & c[0]));
						b[0] <= a[0];
						c[0] <= b[0];
						d[0] <= c[0];
						e[0] <= d[0] + h[0] + (rightrotate(e[0], 6) ^ rightrotate(e[0], 11) ^ rightrotate(e[0], 25)) + ((e[0] & f) ^ ((~e[0]) & g[0])) + k[sha_idx % 16] + w[0][sha_idx % 16];
						f[0] <= e[0];
						g[0] <= f[0];
						h[0] <= g[0];
						w[0][sha_idx % 16] <= w[sha_idx % 16] + w[tstep - 7] + (rightrotate(w[tstep - 15],7) ^ rightrotate(w[tstep - 15],18) ^ (w[tstep - 15] >> 3)) + (rightrotate(w[tstep - 2],17) ^ rightrotate(w[tstep - 2],19) ^ (w[tstep - 2] >> 10));
						sha_idx <= sha_idx + 1;
						state <= COMPUTE;
					end
				end
			endcase
		end
		else if(phase == TWO) begin
			if(compute_sub_state < 26) begin //Because the last two words, w[0][62] and w[0][63] are calculated when compute_state = 24 and compute_state = 25 is used to initialize values of a through h
				case (compute_sub_state)
					0: begin
						//All of these can be set in parallel
						for(int idx = 0; idx < 16; idx++) begin
							w[idx][0] <= memory_blocks[idx][31:0];
							w[idx][1] <= memory_blocks[idx][63:32];
							w[idx][2] <= memory_blocks[idx][95:64];
							w[idx][3] <= memory_blocks[idx][127:96];
							w[idx][4] <= memory_blocks[idx][159:128];
							w[idx][5] <= memory_blocks[idx][191:160];
							w[idx][6] <= memory_blocks[idx][223:192];
							w[idx][7] <= memory_blocks[idx][255:224];
							w[idx][8] <= memory_blocks[idx][287:256];
							w[idx][9] <= memory_blocks[idx][319:288];
							w[idx][10] <= memory_blocks[idx][351:320];
							w[idx][11] <= memory_blocks[idx][383:352];
							w[idx][12] <= memory_blocks[idx][415:384];
							w[idx][13] <= memory_blocks[idx][447:416];
							w[idx][14] <= memory_blocks[idx][479:448];
							w[idx][15] <= memory_blocks[idx][511:480];
						end
						compute_sub_state <= compute_sub_state + 7'd1;
						state <= COMPUTE;
					end
					25: begin //Used to initialize a through h (for each of 16 nonces) so that they can be used in sha256 operation step
						for(int idx = 0; idx < 16; idx++) begin
							a[idx] <= H[idx][0];
							b[idx] <= H[idx][1];
							c[idx] <= H[idx][2];
							d[idx] <= H[idx][3];
							e[idx] <= H[idx][4];
							f[idx] <= H[idx][5];
							g[idx] <= H[idx][6];
							h[idx] <= H[idx][7];
						end
						state <= COMPUTE;
						compute_sub_state <= compute_sub_state + 7'd1;
					end
					default: begin
						for(int idx = 0; idx < 16; idx++) begin
							//two word computations can be parallelized because w[idx] is required to be known for computation of w[idx + 2]
							w[idx][2*compute_sub_state + 7'd14] <= w[idx][2*compute_sub_state - 7'd2] + w[idx][2*compute_sub_state + 7'd7] + (rightrotate(w[idx][2*compute_sub_state - 7'd1],7) ^ rightrotate(w[idx][2*compute_sub_state - 7'd1],18) ^ (w[idx][2*compute_sub_state - 7'd1] >> 3)) + (rightrotate(w[idx][2*compute_sub_state + 7'd12],17) ^ rightrotate(w[idx][2*compute_sub_state + 7'd12],19) ^ (w[idx][2*compute_sub_state + 7'd12] >> 10));
							w[idx][2*compute_sub_state + 7'd15] <= w[idx][2*compute_sub_state - 7'd1] + w[idx][2*compute_sub_state + 7'd8] + (rightrotate(w[idx][2*compute_sub_state],7) ^ rightrotate(w[idx][2*compute_sub_state],18) ^ (w[idx][2*compute_sub_state] >> 3)) + (rightrotate(w[idx][2*compute_sub_state + 7'd13],17) ^ rightrotate(w[idx][2*compute_sub_state + 7'd13],19) ^ (w[idx][2*compute_sub_state + 7'd13] >> 10));
						end
						compute_sub_state <= compute_sub_state + 7'd1;
						state <= COMPUTE;
					end
				endcase
			end
			else if(compute_sub_state < 90) begin //For compute_sub_state >= 26 and <= 89, we peform sha256_op 64 times
				for(int idx = 0; idx < 16; idx++) begin
					a[idx] <= h[idx] + (rightrotate(e[idx], 6) ^ rightrotate(e[idx], 11) ^ rightrotate(e[idx], 25)) + ((e[idx] & f[idx]) ^ ((~(e[idx])) & g[idx])) + k[compute_sub_state - 8'd26] + w[idx][compute_sub_state - 8'd26] + (rightrotate(a[idx], 2) ^ rightrotate(a[idx], 13) ^ rightrotate(a[idx], 22)) + ((a[idx] & b[idx]) ^ (a[idx] & c[idx]) ^ (b[idx] & c[idx]));
					b[idx] <= a[idx];
					c[idx] <= b[idx];
					d[idx] <= c[idx];
					e[idx] <= d[idx] + h[idx] + (rightrotate(e[idx], 6) ^ rightrotate(e[idx], 11) ^ rightrotate(e[idx], 25)) + ((e[idx] & f[idx]) ^ ((~(e[idx])) & g[idx])) + k[compute_sub_state - 8'd26] + w[idx][compute_sub_state - 8'd26];
					f[idx] <= e[idx];
					g[idx] <= f[idx];
					h[idx] <= g[idx];
				end
				compute_sub_state <= compute_sub_state + 7'd1;
				state <= COMPUTE; //Go back to compute if compute_sub_state value is in [26, 89]
			end
			else begin //For compute_sub_state = 90, w0 through w7 (FOR EACH OF 16 NONCES) is set to the final result of this phase's computation
				for(int idx = 0; idx < 16; idx++) begin //This for loop sets w0 through w7 for EACH of 16 nonce values in preparation for phase 3
					//Now, the output hash is directly stored into message bits for phase 3
					w[idx][0] <= H[idx][0] + a[idx];
					w[idx][1] <= H[idx][1] + b[idx];
					w[idx][2] <= H[idx][2] + c[idx];
					w[idx][3] <= H[idx][3] + d[idx];
					w[idx][4] <= H[idx][4] + e[idx];
					w[idx][5] <= H[idx][5] + f[idx];
					w[idx][6] <= H[idx][6] + g[idx];
					w[idx][7] <= H[idx][7] + h[idx];
					//And now, padding
					w[idx][8] <= 32'h80000000;
					w[idx][9] <= 32'h00000000;
					w[idx][10] <= 32'h00000000;
					w[idx][11] <= 32'h00000000;
					w[idx][12] <= 32'h00000000;
					w[idx][13] <= 32'h00000000;
					w[idx][14] <= 32'h00000000;
					w[idx][15] <= 32'd256;
					//Now, hash values will be initialized to the most initial hash values for phase 3
					H[idx][0] <= 32'h6a09e667; 
					H[idx][1] <= 32'hbb67ae85;
					H[idx][2] <= 32'h3c6ef372;
					H[idx][3] <= 32'ha54ff53a;
					H[idx][4] <= 32'h510e527f;
					H[idx][5] <= 32'h9b05688c;
					H[idx][6] <= 32'h1f83d9ab;
					H[idx][7] <= 32'h5be0cd19;
					//Now, I am also going to initialize a through h to this initial hash so that they can be used directly in sha256_operation step without waiting an extra cycle
					a[idx] <= 32'h6a09e667; 
					b[idx] <= 32'hbb67ae85;
					c[idx] <= 32'h3c6ef372;
					d[idx] <= 32'ha54ff53a;
					e[idx] <= 32'h510e527f;
					f[idx] <= 32'h9b05688c;
					g[idx] <= 32'h1f83d9ab;
					h[idx] <= 32'h5be0cd19;
				end
				state <= COMPUTE; //Phase 3 starts from BLOCK stage
				phase <= THREE; //Moving to phase 3 now
				compute_sub_state <= 0; //Because we are moving to COMPUTE state now
			end
		end
		else if(phase == THREE) begin
			//At this stage (entering compute in phase 3), we only need to compute words from word 17 to 64 instead of 1 to 64 as phase 2 has already transferred hash output + padding directly into words 1 to 16
			if(compute_sub_state < 24) begin //Words 17 to 64 are computed for compute_sub_state values 0 to 23
				state <= COMPUTE;
				compute_sub_state <= compute_sub_state + 7'd1; 
				for(int idx = 0; idx < 16; idx++) begin //For loop does it for each nonce value
					w[idx][2*compute_sub_state + 7'd16] <= w[idx][2*compute_sub_state] + w[idx][2*compute_sub_state + 7'd9] + (rightrotate(w[idx][2*compute_sub_state + 7'd1],7) ^ rightrotate(w[idx][2*compute_sub_state + 7'd1],18) ^ (w[idx][2*compute_sub_state + 7'd1] >> 3)) + (rightrotate(w[idx][2*compute_sub_state + 7'd14],17) ^ rightrotate(w[idx][2*compute_sub_state + 7'd14],19) ^ (w[idx][2*compute_sub_state + 7'd14] >> 10));
					w[idx][2*compute_sub_state + 7'd17] <= w[idx][2*compute_sub_state + 7'd1] + w[idx][2*compute_sub_state + 7'd10] + (rightrotate(w[idx][2*compute_sub_state + 7'd2],7) ^ rightrotate(w[idx][2*compute_sub_state + 7'd2],18) ^ (w[idx][2*compute_sub_state + 7'd2] >> 3)) + (rightrotate(w[idx][2*compute_sub_state + 7'd15],17) ^ rightrotate(w[idx][2*compute_sub_state + 7'd15],19) ^ (w[idx][2*compute_sub_state + 7'd15] >> 10));
				end
			end
			else if(compute_sub_state < 88) begin //sha256_op is performed when compute_sub_state is in [24, 87]
				for(int idx = 0; idx < 16; idx++) begin
					a[idx] <= h[idx] + (rightrotate(e[idx], 6) ^ rightrotate(e[idx], 11) ^ rightrotate(e[idx], 25)) + ((e[idx] & f[idx]) ^ ((~(e[idx])) & g[idx])) + k[compute_sub_state - 8'd24] + w[idx][compute_sub_state - 8'd24] + (rightrotate(a[idx], 2) ^ rightrotate(a[idx], 13) ^ rightrotate(a[idx], 22)) + ((a[idx] & b[idx]) ^ (a[idx] & c[idx]) ^ (b[idx] & c[idx]));
					b[idx] <= a[idx];
					c[idx] <= b[idx];
					d[idx] <= c[idx];
					e[idx] <= d[idx] + h[idx] + (rightrotate(e[idx], 6) ^ rightrotate(e[idx], 11) ^ rightrotate(e[idx], 25)) + ((e[idx] & f[idx]) ^ ((~(e[idx])) & g[idx])) + k[compute_sub_state - 8'd24] + w[idx][compute_sub_state - 8'd24];
					f[idx] <= e[idx];
					g[idx] <= f[idx];
					h[idx] <= g[idx];
				end
				state <= COMPUTE;
				compute_sub_state <= compute_sub_state + 7'd1;
			end
			else if (compute_sub_state == 88) begin //compute_sub_state = 88 is used to store final hashes for each nonce into h0 through h7 and prepare to enter write state 
				for(int idx = 0; idx < 16; idx++) begin
					H[idx][0] <= H[idx][0] + a[idx];
					H[idx][1] <= H[idx][1] + b[idx];
					H[idx][2] <= H[idx][2] + c[idx];
					H[idx][3] <= H[idx][3] + d[idx];
					H[idx][4] <= H[idx][4] + e[idx];
					H[idx][5] <= H[idx][5] + f[idx];
					H[idx][6] <= H[idx][6] + g[idx];
					H[idx][7] <= H[idx][7] + h[idx];
				end
				state <= COMPUTE; //Next state is the write state
				compute_sub_state <= compute_sub_state + 7'd1;
			end
			else begin //compute_sub_state = 89 is used to create a one cycle delay so that all final hash values can be written
				compute_sub_state <= 0; //For the future
				state <= WRITE;
				mem_addr <= output_addr;
				mem_we <= 1'b1;
				mem_write_data <= H[0][0]; //0th hash of nonce 0 is written at output_addr
				write_idx <= 0;
			end
		end
	end
	WRITE: begin //We only come to write in phase DONE so no need to check for phase
		case (write_idx)
			0: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[1][0];
			end
			1: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[2][0];
			end
			2: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[3][0];
			end
			3: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[4][0];
			end
			4: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[5][0];
			end
			5: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[6][0];
			end
			6: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[7][0];
			end
			7: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[8][0];
			end
			8: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[9][0];
			end
			9: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[10][0];
			end
			10: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[11][0];
			end
			11: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[12][0];
			end
			12: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[13][0];
			end
			13: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[14][0];
			end
			14: begin
				mem_addr <= mem_addr + 16'd1;
				write_idx <= write_idx + 4'd1;
				state <= WRITE;
				mem_write_data <= H[15][0];
			end
			15: begin
				mem_we <= 1'b0;
				write_idx <= 4'd0; //For the future
				state <= IDLE;
				phase <= DONE;
			end
		endcase
	end
  endcase
end
endmodule
