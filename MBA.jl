using Distributions, DataFrames, Plots

gr()
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
    chosen_arms = zeros(Int, n_sims * horizon)
    rewards = zeros(n_sims * horizon)
    cum_rewards = zeros(n_sims * horizon)
    sim_nums = zeros(n_sims * horizon)
    times = zeros(n_sims * horizon)

    for sim in 1:n_sims
        initialize(algo, length(arms))

        for t = 1:horizon
            index = (sim - 1) * horizon + t

            sim_nums[index] = sim
            times[index] = t

            chosen_arm = select_arm(algo)
            chosen_arms[index] = chosen_arm

            reward = draw(arms[chosen_arm])
            rewards[index] = reward

            if t == 1
                cum_rewards[index] = reward
            else
                cum_rewards[index] = cum_rewards[index-1] + reward
            end

            update(algo, chosen_arm, reward)

        end
    end

    return hcat(sim_nums, times, chosen_arms, rewards, cum_rewards)
end

for epsilon in 0.1:0.1:0.5
    algo = EpsilonGreedyAlgorithm(epsilon, zeros(Int, n_arms), zeros(n_arms))
    results = test_algorithm(algo, arms, 5000, 250)
    results = hcat(repmat([epsilon], size(results, 1), 1), results)
    total_results = vcat(total_results, results)
end
