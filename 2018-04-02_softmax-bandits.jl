using Distributions, DataFrames
using CSV

srand(20130810)

abstract type BanditAlgorithms end

mutable struct Softmax <: BanditAlgorithms
    temperature::Float64
    counts::Vector{Int}
    values::Vector{Float64}
end

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

function Softmax(temperature::Float64, n_arms::Int)
    Softmax(temperature, zeros(Int, n_arms), zeros(n_arms))
end

function initialize(algo::Softmax, n_arms::Int)
    algo.counts = zeros(Int, n_arms)
    algo.values = zeros(n_arms)
end

function select_arm(algo::Softmax)
    z = sum(exp(algo.values ./ algo.temperature))
    probs = exp(algo.values ./ algo.temperature) ./ z

    return rand(Categorical(probs))
end

function update(algo::Softmax, chosen_arm::Int, reward::Real)
    algo.counts[chosen_arm] += 1
    n = algo.counts[chosen_arm]

    value = algo.values[chosen_arm]
    algo.values[chosen_arm] = ((n - 1)/n) * value + (1/n) * reward
end

means = [0.1, 0.1, 0.1, 0.1, 0.9]
n_arms = length(means)
shuffle!(means)

arms = map(mu -> BernoulliArm(mu), means)

println("Best arm is $(indmax(means))")

function test_algorithm(algo::Softmax, arms::Vector, n_sims::Int, horizon::Int)
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
                          reward = Float64[], temperature = Float64[])

for temperature in 0.1:0.1:0.5
  algo = Softmax(temperature, n_arms)
  results = test_algorithm(algo, arms, 5000, 250)
  results[:temperature] = fill(temperature, nrow(results))
  append!(total_results, results)
end
