using Distributions, DataFrames
using Feather

srand(20130810)

abstract type BanditAlgorithm end

mutable struct AnnealingSoftmax <: BanditAlgorithm
    counts::Vector{Int}
    values::Vector{Float64}
end

function AnnealingSoftmax(n_arms::Int)
    AnnealingSoftmax(zeros(Int, n_arms), zeros(n_arms))
end

function initialize(algo::AnnealingSoftmax, n_arms::Int)
    algo.counts = zeros(Int, n_arms)
    algo.values = zeros(n_arms)
end

function select_arm(algo::AnnealingSoftmax)
    t = sum(algo.counts) + 1
    temperature = 1 / log(t + eps(1.0))

    z = sum(exp(algo.values ./ temperature))
    probs = exp(algo.values ./ temperature) ./ z

    return rand(Categorical(probs))
end

function update(algo::AnnealingSoftmax, chosen_arm::Int, reward::Real)
    algo.counts += 1
    n = algo.counts[chosen_arm]

    value = algo.values[chosen_arm]
    algo.values[chosen_arm] = ((n - 1)/n) * value + (1/n) * reward

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

function test_algorithm(algo::AnnealingSoftmax, arms::Vector, n_sims::Int, horizon::Int)
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

# Case 1: There is a large difference in the A/B test

means = [0.1, 0.1, 0.1, 0.1, 0.9]
n_arms = length(means)

shuffle!(means)
arms = map(mu -> BernoulliArm(mu), means)

println("Best arm is: $(indmax(means))")

algo = AnnealingSoftmax(zeros(Int, n_arms), zeros(n_arms))

@time results = test_algorithm(algo, arms, 5000, 250)

# Case 2: There is minute difference in the A/B

means = [0.1, 0.1, 0.1, 0.1, 0.12]
n_arms = length(means)

shuffle!(means)
arms = map(mu -> BernoulliArm(mu), means)

println("Best arm is: $(indmax(means))")

algo = AnnealingSoftmax(zeros(Int, n_arms), zeros(n_arms))

@time results = test_algorithm(algo, arms, 5000, 250)

@time Feather.write("C:\\Users\\kimmcodxb\\Documents\\GitHub\\AnalysisofExperiments\\data\\2018-04-04_annealing-softmax-results-nodiff.feather",
                    results)
