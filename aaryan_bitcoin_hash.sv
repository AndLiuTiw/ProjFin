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
logic [5:0] compute_sub_state;

//logic [ 4:0] state; //Was given in starter code, commented out by me
logic [31:0] hout[num_nonces];

//Instead of h0 to h7 like in part 1, I am using H[i] as hash value in part 2
logic [31:0] H[16][8]; //16 because one for each nonce, 8 because H0 to H7 requird for each computation, only H[0] is used for phase one 
logic [31:0] a, b, c, d, e, f, g, h;

logic [31:0] message[16][20]; //These are the 20 message blocks, each one is 32 bits, 1 required for each nonce, message[0] i sused for phase one

//Array because 1 required for each nonce for one or more phases
logic [31:0] w[16][64]; //16 because one for each nonce, 64 because 64 words are required in word expansion, only w[0] is used for phase one

logic [5:0] read_idx; //Index used to read values from memory, goes from 0 to 19;

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
	IDLE: begin
		if (start) begin
			//initialized to initial hash values
			if (phase == ONE) begin
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
			else if (phase == TWO) begin
			end
			else if (phase == THREE) begin
			end
			else if (phase == DONE) begin
			end
		end
	end
	READ: begin //READ is performed only once, so no need to check phase
		if(read_idx > 0) begin
			message[0][read_idx - 1] <= mem_read_data;
		end
		if(read_idx <= 19) begin
			mem_addr <= mem_addr + 16'd1; //To read from next address in memory
			read_idx <= read_idx + 8'd1;
			state <= READ;
		end
		else begin
			read_idx <= 0; //Reset it for the future
			state <= BLOCK;
		end
	end
	BLOCK: begin
		if(phase == ONE) begin
			memory_blocks[0] <= {message[0][15],message[0][14],message[0][13],message[0][12],message[0][11],message[0][10],message[0][9],message[0][8],message[0][7],message[0][6],message[0][5],message[0][4],message[0][3],message[0][2],message[0][1],message[0][0]};
			state <= COMPUTE;
			compute_sub_state <= 0; //Because in this state, we need to set values from 
		end
		else if(phase == TWO) begin
		end
		else if(phase == THREE) begin
		end
		else if (phase == DONE) begin
		end
	end
	COMPUTE: begin
		if(phase == ONE) begin
			if(compute_sub_state < 25) begin //Because the last two words, w[0][62] and w[0][63] are calculated when compute_state = 24
				case (compute_sub_state)
					0: begin
						//All of these can be set in parallel
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
						compute_sub_state <= compute_sub_state + 6'b1;
						state <= COMPUTE;
					end
					default: begin //two word computations can be parallelized because w[idx] is required to be known for computation of w[idx + 2]
						w[0][2*compute_sub_state + 6'd14] <= w[0][2*compute_sub_state - 6'd2] + w[0][2*compute_sub_state + 6'd7] + (rightrotate(w[0][2*compute_sub_state - 6'd1],7) ^ rightrotate(w[0][2*compute_sub_state - 6'd1],18) ^ (w[0][2*compute_sub_state - 6'd1] >> 3)) + (rightrotate(w[0][2*compute_sub_state + 6'd12],17) ^ rightrotate(w[0][2*compute_sub_state + 6'd12],19) ^ (w[0][2*compute_sub_state + 6'd12] >> 10));
						w[0][2*compute_sub_state + 6'd15] <= w[0][2*compute_sub_state - 6'd1] + w[0][2*compute_sub_state + 6'd8] + (rightrotate(w[0][2*compute_sub_state],7) ^ rightrotate(w[0][2*compute_sub_state],18) ^ (w[0][2*compute_sub_state] >> 3)) + (rightrotate(w[0][2*compute_sub_state + 6'd13],17) ^ rightrotate(w[0][2*compute_sub_state + 6'd13],19) ^ (w[0][2*compute_sub_state + 6'd13] >> 10));
						compute_sub_state <= compute_sub_state + 6'b1;
						state <= COMPUTE;
					end
				endcase
			end
			else begin //For compute_sub_state >= 25 and < ..., we peform sha256_op 64 times
			end
		end
		else if(phase == TWO) begin
		end
		else if(phase == THREE) begin
		end
		else if(phase == DONE) begin
		end
	end
	WRITE: begin //We only come to write in phase DONE so no need to check for phase
	end
  endcase
end
endmodule
