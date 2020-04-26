namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    public class Pattern : IPattern
    {
        public ulong PatternAttributes { get; set; }

        public MorphemeRule MorphemeMatchingRule { get; set; }

        /// <summary>
        /// Rules for the matching adposition.
        /// </summary>
        public PatternRule AdPositionMatchingRule { get; set; }

        /// <summary>
        /// Rules for the matching left child.
        /// </summary>
        public PatternRule LeftMatchingRule { get; set; }

        /// <summary>
        /// Rules for the matching right child.
        /// </summary>
        public PatternRule RightMatchingRule { get; set; }

        /// <summary>
        /// Rules for the matching governor.
        /// </summary>
        public PatternRule GovernorMatchingRule { get; set; }

        public int ValencyPosition => PatternAttributesArrangement.PatternAttributes.ValencyPosition.GetValencyPosition(PatternAttributes);
    }
}
