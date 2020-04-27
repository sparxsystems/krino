namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    public class Pattern : IPattern
    {
        public ulong PatternAttributes { get; set; }

        public MorphemeRule MorphemeRule { get; set; }

        /// <summary>
        /// Rules for the matching adposition.
        /// </summary>
        public PatternRule AdPositionRule { get; set; }

        /// <summary>
        /// Rules for the matching left child.
        /// </summary>
        public PatternRule LeftRule { get; set; }

        /// <summary>
        /// Rules for the matching right child.
        /// </summary>
        public PatternRule RightRule { get; set; }

        /// <summary>
        /// Rules for the matching governor.
        /// </summary>
        public PatternRule GovernorRule { get; set; }

        public int ValencyPosition => PatternAttributesArrangement.PatternAttributes.ValencyPosition.GetValencyPosition(PatternAttributes);
    }
}
