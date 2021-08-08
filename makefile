.DEFAULT_GOAL = build

build:
	stack install --fast --test

update:
	morpheus build examples/source/simple.gql examples/rendering/Simple.hs
	morpheus build examples/source/mutation.gql examples/rendering/Mutation.hs
	morpheus build examples/source/subscription.gql examples/rendering/Subscription.hs

