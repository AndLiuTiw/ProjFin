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

//logic [ 4:0] state; //Was given in starter code, commented out by me
logic [31:0] hout[num_nonces];

//Instead of h0 to h7 like in part 1, I am using H[i] as hash value in part 2
logic [31:0] H[128]; 
logic [31:0] a, b, c, d, e, f, g, h;

//Array because 1 required for each nonce for one or more phases
logic [31:0] w0[16];
logic [31:0] w1[16];
logic [31:0] w2[16];
logic [31:0] w3[16];
logic [31:0] w4[16];
logic [31:0] w5[16];
logic [31:0] w6[16];
logic [31:0] w7[16];

//Not an array because only one required for all phases
logic [31:0] w8;
logic [31:0] w9;
logic [31:0] w10;
logic [31:0] w11;
logic [31:0] w12;
logic [31:0] w13;
logic [31:0] w14;
logic [31:0] w15;

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

// Student to add rest of the code here
always_ff @(posedge clk, negedge reset_n)
begin
	if (!reset_n) begin
    mem_we <= 1'b0; //Because we don't want to write anything in the beginning 
    state <= IDLE; //The first state to go to
	 phase <= ONE; //Always start at phase 1
	 mem_addr <= message_addr; //Because we will be reading first 
  end 
  else case (state)
	IDLE: begin
		if (start) begin
			//initialized to initial hash values
			if (phase == ONE) begin
				H[0] <= 32'h6a09e667; 
				H[1] <= 32'hbb67ae85;
				H[2] <= 32'h3c6ef372;
				H[3] <= 32'ha54ff53a;
				H[4] <= 32'h510e527f;
				H[5] <= 32'h9b05688c;
				H[6] <= 32'h1f83d9ab;
				H[7] <= 32'h5be0cd19;
				state <= READ;
				mem_we <= 1'b0; //Nothing to write when going to next state
				mem_addr <= message_addr; //Because we will be reading first
			end
			else if (phase == TWO) begin
				//The output hash of phase one at the end of computation will be stored in H[0] to H[7], this needs to be spread to the other H indices
				for(int i = 8; i < 128; i++) begin
					H[i] <= H[i % 8]; //Because for all 16 nonces, hash values are same
				end
				state <= READ;
				mem_we <= 1'b0; //Nothing to write when going to next state
				mem_addr <= message_addr; //Because we will be reading first
			end
			else if (phase == THREE) begin
				//Input hashes are same as in phase 1, except we need to do it for each of the 16 nonces
				//IMP NOTE: The below statements change hash values and so the output of phase 2 is NOT RETAINED, therefore, it must be ensured that those values are stored somewhere before progressing to this stage
				for(int i = 0; i < 8; i++) begin
					H[i*8] <= 32'h6a09e667;
					H[i*8 + 1] <= 32'hbb67ae85;
					H[i*8 + 2] <= 32'h3c6ef372;
					H[i*8 + 3] <= 32'ha54ff53a;
					H[i*8 + 4] <= 32'h510e527f;
					H[i*8 + 5] <= 32'h9b05688c;
					H[i*8 + 6] <= 32'h1f83d9ab;
					H[i*8 + 7] <= 32'h5be0cd19;
					state <= READ;
					mem_we <= 1'b0; //Nothing to write when going to next state
					mem_addr <= message_addr; //Because we will be reading first
				end
			end
			else if (phase == DONE) begin
				//H[0], H[8], H[16], ... , H[112], H[120] contain the hash values to be written to memory
				state <= WRITE; //Now, we need to write these 16 hashes to memory
				mem_we <= 1'b1; //Because next state is WRITE
				mem_addr <= output_addr; //Because next state is WRITE
				mem_write_data <= H[0]; //H[0] is the first hash to be written to memory
			end
		end
	end
	READ: begin
		if(phase == ONE) begin
		end
		else if(phase == TWO) begin
		end
		else if(phase == THREE) begin
		end
		else if (phase == DONE) begin
		end
	end
	BLOCK: begin
	end
	COMPUTE: begin
	end
	WRITE: begin
	end
  endcase
end

endmodule
