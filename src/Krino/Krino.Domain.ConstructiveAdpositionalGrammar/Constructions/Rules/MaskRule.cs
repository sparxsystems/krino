using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Evaluates true if the value contains the required bit mask.
    /// </summary>
    [DebuggerDisplay("{Mask}")]
    public class MaskRule : IRule<BigInteger>
    {
        public static IRule<BigInteger> Anything => RuleMaker.Anything<BigInteger>();

        public static IRule<BigInteger> Nothing => RuleMaker.Nothing<BigInteger>();

        public static IRule<BigInteger> Something => Anything.And(RuleMaker.IsNot<BigInteger>(0));

        public static MaskRule Is(BigInteger mask) => new MaskRule(mask);

        public MaskRule(BigInteger mask)
        {
            Mask = mask;
        }

        public BigInteger Mask { get; private set; }

        public bool Evaluate(BigInteger value) => EnumBase.IsIn(Mask, value);


        public bool Equals(IRule<BigInteger> other) => other is MaskRule maskRule && Mask == maskRule.Mask;

        public static IRule<BigInteger> operator &(MaskRule mask1, IRule<BigInteger> rule2) => mask1.And(rule2);

        public static IRule<BigInteger> operator |(MaskRule mask1, IRule<BigInteger> rule2) => mask1.Or(rule2);

        public static IRule<BigInteger> operator !(MaskRule mask) => mask.Not(); 
    }
}
