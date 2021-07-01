using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules
{
    /// <summary>
    /// Evaluates true if the value contains the required bit mask.
    /// </summary>
    [DebuggerDisplay("{DebugView}")]
    public class MaskRule : RuleBase<BigInteger>, IValueRule<BigInteger>
    {
        public static IRule<BigInteger> Anything => RuleMaker.Anything<BigInteger>();

        public static IRule<BigInteger> Nothing => RuleMaker.Nothing<BigInteger>();

        public static IRule<BigInteger> Something => Anything.And(RuleMaker.IsNot<BigInteger>(0));

        public static MaskRule Is(BigInteger mask) => new MaskRule(mask);

        public MaskRule(BigInteger mask)
        {
            Value = mask;
        }

        public BigInteger Value { get; private set; }

        public override bool Evaluate(BigInteger value) => EnumBase.IsIn(Value, value);

        public override bool Equals(IRule<BigInteger> other) => other is MaskRule maskRule && Value == maskRule.Value;


        private string DebugView => Value.ToString();
    }
}
