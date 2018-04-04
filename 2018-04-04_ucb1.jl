using Distributions, DataFrames, Feather

srand(20130810)

abstract type BanditAlgorithm end

mutable struct UCB <: BanditAlgorithm
    counts::Vector{Int}
    values::Vector{Float64}
end

function UCB(n_arms::Int)
    UCB(zeros(Int, n_arms), zeros(n_arms))
end

function initialize(algo::UCB, n_arms::Int)
    algo.counts = zeros(Int, n_arms)
    algo.values = zeros(n_arms)
end

function select_arm(algo::UCB)
    if any(algo.counts .== 0)
        return find(algo.counts .== 0)[1] # Because find returns a vector
    else
        ucb_values = algo.values + sqrt((2 * log(sum(algo.counts))) ./ algo.counts)
        return indmax(ucb_values)
    end
end

function update(algo::UCB, chosen_arm::Int, reward::Real)
  algo.counts[chosen_arm] += 1
  n = algo.counts[chosen_arm]

  value = algo.values[chosen_arm]
  algo.values[chosen_arm] = ((n - 1) / n) * value + (1 / n) * reward
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

function test_algorithm(algo::UCB, arms::Vector, n_sims::Int, horizon::Int)
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

means = [0.1, 0.1, 0.1, 0.1, 0.9]
n_arms = length(means)
shuffle!(means)
arms = map(mu -> BernoulliArm(mu), means)
println("Best arm is $(indmax(means))")

algo = UCB(zeros(Int, n_arms), zeros(n_arms))
@time results = test_algorithm(algo, arms, 5000, 250)

@time Feather.write("data\\2018-04-04_ucb-results.feather",
                    results)
