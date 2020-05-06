using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Evaluates true if the value contains the required bit mask.
    /// </summary>
    [DebuggerDisplay("{myMask}")]
    public class MaskRule : IRule<BigInteger>
    {
        public static MaskRule Is(BigInteger mask) => new MaskRule(mask);

        private BigInteger myMask;

        public MaskRule(BigInteger mask)
        {
            myMask = mask;
        }

        public bool Evaluate(BigInteger value) => EnumBase.IsIn(myMask, value);


        public bool Equals(IRule<BigInteger> other) => other is MaskRule maskRule && myMask == maskRule.myMask;
    }
}
