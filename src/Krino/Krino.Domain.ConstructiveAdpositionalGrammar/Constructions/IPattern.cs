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
        ulong PatternAttributes { get; }

        /// <summary>
        /// Rules for the matching morpheme.
        /// </summary>
        MorphemeRule MorphemeMatchingRule { get; }

        /// <summary>
        /// Rules for the matching adposition.
        /// </summary>
        PatternRule AdPositionMatchingRule { get; }

        /// <summary>
        /// Rules for the matching left child.
        /// </summary>
        PatternRule LeftMatchingRule { get; }

        /// <summary>
        /// Rules for the matching right child.
        /// </summary>
        PatternRule RightMatchingRule { get; }

        /// <summary>
        /// Rules for the matching governor.
        /// </summary>
        PatternRule GovernorMatchingRule { get; }

        /// <summary>
        /// Returns the valency position.
        /// </summary>
        int ValencyPosition { get; }
    }
}
