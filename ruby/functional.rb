# This module defines methods and operators for functional
# programming.
module Functional

  # Apply this function to each element of the specified Enumerable,
  # returning an array of results. This is the reverse of
  # Enumerable.map
  # Use | as an operator alias. Read "|" as "over" or "applied over".
  #
  # Example:
  # a = [[1,2],[3,4]]
  # sum = lambda {|x,y| x+y}
  # sums = sum|a    => [3,7]
  def apply(enum)
    enum.map &self
  end
  alias | apply

  # Use this function to "reduce" an enumerable to a single
  # quantity. This is the inverse of Enumerable.inject
  # Use <= as an operator alias.
  # Mnemonic: <= looks like a needle for injection
  # Example:
  #   data = [1,2,3,4]
  #   sum = lambda {|x,y| x+y}
  #   total = sum<=data    => 10
  def reduce(enum)
    enum.inject &self
  end
  alias <= reduce

  # Return a new lambda that computes self[f[args]]. Use * as an operator
  # alias for compose. Examples, using the * alias for this method.
  #
  # f = lambda{|x| x*x}
  # g = lambda{|x| x+1}
  # (f*g)[2]  => 9
  # (g*f)[2]  => 5
  def compose(f)
    if self.respond_to?(:arity) && self.arity == 1
      lambda {|*args| self[f[*args]] }
    else
      lambda {|*args| self[*f[*args]] }
    end
  end
  # * is the natural operator for function composition.
  alias * compose

  # Return a lambda equivalent to this one wiht one or more initial
  # arguments applied. When only a single argument is being specified,
  # the >> alias may be simpler to use.
  # Example:
  #   product = lambda {|x,y| x*y}
  #   doubler = product >> 2
  #
  def apply_head(*first)
    lambda {|*rest| self[*first.concat(rest)]}
  end

  #
  # Return a lambda equivalent to this one with one or more final arguments
  # applied. When only a single argument is being specified, the <<
  # alias may be simpler.
  # Example:
  #   difference = lambda {|x,y| x-y}
  #   decrement = difference << 1
  #
  def apply_tail(*last)
    lambda {|*rest| self[*rest.concat(last)]}
  end

  # Here are operator alternatives for these methods. The angle
  # brackets point to the side on which the argument is shifted in.
  alias >> apply_head # g = f >> 2 -- set first argument to 2
  alias << apply_tail # g = f << 2 -- set last argument to 2

  #
  # Return a new lambda that caches the results of this function and only
  # calls the function when new arguments are supplied.
  def memoize
    cache = {} # An empty cache. The lambda captures this in its closure.
    lambda {|*args|
      # notice that the hash key is the entire array of arguments!
      unless cache.has_key?(args) # if no cached result for these args
        cache[args] = self[*args] # compute and cache the result
      end
      cache[args]                 # return the result from cache
    }
  end
  # A (probably unnecessary) unary + operator for memoization
  # Mnemonic: the + operator means "improved"
  alias +@ memoize # cached_f = +f
end

# Add these functional programming methods to Proc and Method classes.
class Proc; include Functional; end
class Method; include Functional; end

# Example code
# Compute the average and standard deviation of an array of numbers
a = [1,2,3,4,5,6]
mean = a.inject {|x,y| x+y} / a.size
sumOfSquares = a.map {|x| (x-mean)**2}.inject {|x,y| x+y}
standardDeviation = Math.sqrt(sumOfSquares / (a.size-1))


# With apply and reduce:
sum = lambda {|x,y| x+y}
mean = (sum<=a)/a.size
deviation = lambda {|x| x-mean}
square = lambda {|x| x*x}
standardDeviation=Math.sqrt((sum <= square|(deviation|a))/(a.size-1))

# Using function composition:
standardDeviation = Math.sqrt((sum<=square*deviation|a)/(a.size-1))

# Using partial application
difference = lambda {|x,y| x-y}
deviation = difference << mean





