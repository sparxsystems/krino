using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    /// <summary>
    /// Declares the pattern which gives the form to the linguistic construction.
    /// </summary>
    public interface IPattern
    {
        /// <summary>
        /// Attributes of this pattern.
        /// </summary>
        ulong Attributes { get; }

        /// <summary>
        /// Rules for the matching morpheme.
        /// </summary>
        MorphemeRule MorphemeMatchingRule { get; }

        /// <summary>
        /// Rules for the matching adposition.
        /// </summary>
        PatternRule AdPositionPatternMatchingRule { get; }

        /// <summary>
        /// Rules for the matching left child.
        /// </summary>
        PatternRule LeftPatternMatchingRule { get; }

        /// <summary>
        /// Rules for the matching right child.
        /// </summary>
        PatternRule RightPatternMatchingRule { get; }

        /// <summary>
        /// Rules for the matching governor.
        /// </summary>
        PatternRule GovernorPatternMatchingRule { get; }

        /// <summary>
        /// Returns the valency position.
        /// </summary>
        int ValencyPosition { get; }
    }
}
