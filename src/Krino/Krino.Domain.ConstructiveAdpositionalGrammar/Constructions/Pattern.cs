using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    [DebuggerDisplay("{LeftRule} <- {AdPositionRule} -> {RightRule}", Name = "{myName}")]
    public class Pattern : IPattern
    {
        // Optional information for the debugging purposes.
        private string myName;

        public Pattern(string name = null)
        {
            myName = name;
        }

        public ulong PatternAttributes { get; set; }

        public MorphemeRule MorphemeRule { get; set; } = MorphemeRule.Anything;

        /// <summary>
        /// Rules for the matching left child.
        /// </summary>
        public PatternRule LeftRule { get; set; } = PatternRule.Anything;

        /// <summary>
        /// Rules for the matching right child.
        /// </summary>
        public PatternRule RightRule { get; set; } = PatternRule.Anything;


        public int ValencyPosition => PatternAttributesArrangement.PatternAttributes.ValencyPosition.GetValencyPosition(PatternAttributes);
    }
}
