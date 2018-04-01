using Distributions, DataFrames
using CSV

srand(20130810)

abstract type BanditAlgorithms end

mutable struct EpsilonGreedyAlgorithm <: BanditAlgorithms
    epsilon::Float64
    counts::Vector{Int}
    values::Vector{Float64}
end

function EpsilonGreedyAlgorithm(epsilon::Float64, n_arms::Int)
    EpsilonGreedyAlgorithm(epsilon, zeros(Int, n_arms), zeros(Float64, n_arms))
end

function initialize(algo::EpsilonGreedyAlgorithm, n_arms::Int)
    algo.counts = zeros(Int, n_arms)
    algo.values = zeros(Float64, n_arms)
end

function initialize(algo::EpsilonGreedyAlgorithm, arms::Vector)
    initialize(algo, length(arms))
end

function select_arm(algo::EpsilonGreedyAlgorithm)
    if rand() > algo.epsilon
        return indmax(algo.values)
    else
        return rand(1:length(algo.values))
    end
end

function update(algo::EpsilonGreedyAlgorithm, chosen_arm::Int, reward::Real)
    algo.counts[chosen_arm] += 1
    n = algo.counts[chosen_arm]

    value = algo.values[chosen_arm]

    algo.values[chosen_arm] = ((n-1)/n) * value + (1/n) * reward
end

#===
testing the algorithm
==#

abstract type BanditArm end

struct BernoulliArm <: BanditArm
    p::Float64
end

function draw(arm::BernoulliArm)
    if rand() > arm.p
        return 0
    else
        return 1
    end
end


means = [0.1, 0.1, 0.1, 0.1, 0.9] # Five arms each with respective click-through rates
n_arms = length(means)
shuffle!(means)

arms = map(mu -> BernoulliArm(mu), means)

function test_algorithm(algo::EpsilonGreedyAlgorithm, arms::Vector, n_sims::Int, horizon::Int)
    results = DataFrame(sim_num = Int[], time = Int[], chosen_arm = Int[],
                        reward = Float64[])

    for sim in 1:n_sims
        initialize(algo, length(arms))

        for t = 1:horizon
            chosen_arm = select_arm(algo)

            reward = draw(arms[chosen_arm])

            update(algo, chosen_arm, reward)

            push!(results, [sim, t, chosen_arm, reward])
        end
    end

    return results
end

total_results = DataFrame(sim_num = Int[], time = Int[], chosen_arm = Int[],
                          reward = Float64[], epsilon = Float64[])

for epsilon in 0.1:0.1:0.5
    algo = EpsilonGreedyAlgorithm(epsilon, zeros(Int, n_arms), zeros(n_arms))
    results = test_algorithm(algo, arms, 5000, 250)
    results[:epsilon] = fill(epsilon, nrow(results))
    append!(total_results, results)
end
