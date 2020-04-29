using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Evaluates true if the value contains the required bit mask.
    /// </summary>
    [DebuggerDisplay("{myMask}")]
    public class MaskRule : IRule<ulong>
    {
        private ulong myMask;

        public MaskRule(ulong mask)
        {
            myMask = mask;
        }

        public bool Evaluate(ulong value) => EnumBase.IsIn(myMask, value);


        public bool Equals(IRule<ulong> other) => other is MaskRule maskRule && myMask == maskRule.myMask;


        public static MaskRule Is(ulong mask) => new MaskRule(mask);
    }
}
