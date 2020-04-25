using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    public class Pattern : IPattern
    {

        public ulong Attributes { get; set; }

        public MorphemeRule MorphemeMatchingRule { get; set; }

        /// <summary>
        /// Rules for the matching adposition.
        /// </summary>
        public PatternRule AdPositionPatternMatchingRule { get; set; }

        /// <summary>
        /// Rules for the matching left child.
        /// </summary>
        public PatternRule LeftPatternMatchingRule { get; set; }

        /// <summary>
        /// Rules for the matching right child.
        /// </summary>
        public PatternRule RightPatternMatchingRule { get; set; }

        /// <summary>
        /// Rules for the matching governor.
        /// </summary>
        public PatternRule GovernorPatternMatchingRule { get; set; }

        public int ValencyPosition => PatternAttributes.ValencyPosition.GetValencyPosition(Attributes);
    }
}
