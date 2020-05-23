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
    public class MaskRule : RuleBase<BigInteger>, IReferenceValueRule<BigInteger>
    {
        public static IRule<BigInteger> Anything => RuleMaker.Anything<BigInteger>();

        public static IRule<BigInteger> Nothing => RuleMaker.Nothing<BigInteger>();

        public static IRule<BigInteger> Something => Anything.And(RuleMaker.IsNot<BigInteger>(0));

        public static MaskRule Is(BigInteger mask) => new MaskRule(mask);

        public MaskRule(BigInteger mask)
        {
            ReferenceValue = mask;
        }

        public BigInteger ReferenceValue { get; private set; }

        public override bool Evaluate(BigInteger value) => EnumBase.IsIn(ReferenceValue, value);

        public override bool Equals(IRule<BigInteger> other) => other is MaskRule maskRule && ReferenceValue == maskRule.ReferenceValue;
    }
}
