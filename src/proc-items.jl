# Charlie Redmon
# 2025-08-07

using CSV
using DataFrames
using StringDistances 

# directories
d = CSV.read("items.csv", DataFrame);

# find minimal pairs and add to data 
el_spkr1 = [];
el_spkr2 = [];
el_rep1 = [];
el_rep2 = [];
el_dist = [];
el_diff = [];
el_phon1 = [];
el_phon2 = [];
el_fn1 = [];
el_fn2 = [];

for i=1:(nrow(d)-1)

    for j=(i+1):nrow(d)

        phon_i = d[i, :Phon];
        phon_j = d[j, :Phon];

        dist_ij = evaluate(Hamming(), phon_i, phon_j)

        if dist_ij < 2

            push!(el_spkr1, d[i, :Speaker]);
            push!(el_spkr2, d[j, :Speaker]);

            push!(el_rep1, d[i, :Rep]);
            push!(el_rep2, d[j, :Rep]);

            push!(el_dist, dist_ij);

            push!(el_phon1, phon_i);
            push!(el_phon2, phon_j);

            if dist_ij == 0
                push!(el_diff, missing);
            else
                ivec = split(phon_i, "");
                jvec = split(phon_j, "");
                
                ijdiff = ivec .!= jvec;
                
                cx = [ivec[ijdiff][1], jvec[ijdiff][1]];
                sort!(cx);

                push!(el_diff, join(cx, "-"));
            end

        end

    end

end

el = DataFrame(Speaker1=el_spkr1, Speaker2=el_spkr2, Rep1=el_rep1, Rep2=el_rep2, 
               Phon1=el_phon1, Phon2=el_phon2, Dist=el_dist, Contrast=el_diff);

el
first(el, 4)
last(el, 4)


