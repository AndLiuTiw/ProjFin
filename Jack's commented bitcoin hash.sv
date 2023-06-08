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
logic [31:0] H[7:0]; 
logic [31:0] Hout [15:0]; 			//place to store H[0] from each nonce
logic [31:0] nonce ;
logic [31:0] a, b, c, d, e, f, g, h;		
logic [31:0] aa, bb, cc, dd, ee, ff, gg, hh;	//stored output hashses from phase 2 to be used as inputs to phase 3

logic [511:0] memory_block;

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


logic [4:0] read_idx; //Index used to read values from memory, goes from 0 to 15;

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
	 read_idx <= 0; //0 is obviously the reset value
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
				nonce <= -1;		//so that for first iteration of phase 2 nonce goes to 0
				state <= READ;
				mem_we <= 1'b0; //Nothing to write when going to next state
				mem_addr <= message_addr; //Because we will be reading first
				read_idx <= 0; //Because we need to fill from w0 to w15
			end
			else if (phase == TWO) begin
				//The output hash of phase one at the end of computation will be stored in H[0] to H[7], this needs to be spread to the other H indices
				H[0] <= aa;
				H[1] <= bb;
				H[2] <= cc;
				H[3] <= dd;
				H[4] <= ee;
				H[5] <= ff;
				H[6] <= gg;
				H[7] <= hh;
				nonce <= nonce + 1;
				if (nonce == 15) begin
					nonce <= 0;
					state <= WRITE;
				end
				else begin
					state <= READ;
				end
				mem_we <= 1'b0; //Nothing to write when going to next state
				mem_addr <= message_addr + 16'd16; //Because we will be reading 17th to 20th word right now (17th word is at message_addr + 16)
				read_idx <= 0; //Because we need to fill from w0 to w15
			end
			else if (phase == THREE) begin
				//Input hashes are same as in phase 1, except we need to do it for each of the 16 nonces
				//IMP NOTE: The below statements change hash values and so the output of phase 2 is NOT RETAINED, therefore, it must be ensured that those values are stored somewhere before progressing to this stage
				
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
					mem_addr <= message_addr; //Because we will be reading first (don't really care about this because we are not reading from memory for phase three)
					read_idx <= 0; //Because we need to start filling from w0 to w15
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
		if(phase == ONE) begin			//does this syntax work?
			case (read_idx)
				0: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				1: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				2: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				3: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				4: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				5: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				6: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				7: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				8: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				9: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				10: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
			        11: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				12: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				13: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				14: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				15: begin
					message[read_idx] <= mem_read_data;
					mem_addr <= mem_addr + 1; //because we do not need to go any further, so in preparation for phase 2, set it to this even though the exact same thing is done in IDLE state for phase 2 as well
					read_idx <= read_idx + 1;//In preparation for phase 2
				end
			endcase
		end
		else if(phase == TWO) begin
		case (read_idx)
				16: begin
					message[0] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				17: begin
					message[1] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
				18: begin
					message[2] <= mem_read_data;
					mem_addr <= mem_addr + 1;
					read_idx <= read_idx + 1;
				end
		endcase
		end		
		
		else if(phase == THREE) begin
							// 3 doesnt need to read anything from memory  here for readability
		end
		else if (phase == DONE) begin
		
		end
		state <= BLOCK;
	end
	BLOCK: begin
		if(phase == ONE) begin		//Initial Hashs for hashes && 1st 16 words of message
			memory_block <= message[15:0];
			i <= 1;
			state <= COMPUTE;
		end
		else if(phase == TWO) begin	//Hashs from Phase 1 for hashes && last 3 words of message && padding and nonce-ing	 
					
			memory_block <= {32'd640,{10{32'h00000000}},32'h80000000,nonce,message[18:16]};
			i <= 1;
			state <= COMPUTE;
		end
		else if(phase == THREE) begin	//Initial Hashs for hashes  1 && output hashes from phase TWO for message
			memory_block <= {32'd256,{6{32'h00000000}},32'h80000000,hh,gg,ff,ee,dd,cc,bb,aa};
			i <= 1;
			state <= COMPUTE;
		end
	end

	COMPUTE: begin
	 if (i <= 64) begin //For i values from 1 to 64, this is the word expansion part		//should be unchanged from Aaryan's code
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
		  else if (i == 129) begin //For i value 129	
		   //a through h are going to be used again BLOCK state to initialize h0 to h7
		   a <= a + h0;
			b <= b + h1;		
			c <= c + h2;
			d <= d + h3;
			e <= e + h4;
			f <= f + h5;
			g <= g + h6;
			h <= h + h7;
			i <= i + 1;
			state <= COMPUTE;
		  end
		  else begin
			if(phase == ONE) begin			//NOT needed, just here for readabilty
				phase <= TWO;
				aa <= a;
				bb<= b;				
				cc<= c;
				dd<= d;
				ee<= e;
				ff<= f;
				gg<= g;
				hh<= h;	
		   	end
			else if (phase == TWO) begin	// this should only be ran if we are in phase 2  
				
				phase <= THREE;
			end
			else if (phase == THREE) begin	// this should only be ran if we are in phase 2  
				Hout[nonce] <= a;
				phase <= TWO;
				
			end
			state <= IDLE; //Go to IDLE if state if i value is 130 		(because IDLE now does processsing)
		  end 
    end	
	end
	WRITE: begin
		if (nonce < 16) begin
			cur_write_data <= Hout[nonce];		//I'm not very knowledgeable about this section
			nonce <= nonce + 32'd1;
			//offset <= offset + 16'd1;
			state <= WRITE;
		end
		else  begin
			cur_we <= 0; //Because next state is IDLE state
			nonce <= 32'd0;
			state <= DONE;
			//offset <= 0;
		end
	end
	DONE: begin
	end
  endcase
end
assign done = (state == DONE);
endmodule
