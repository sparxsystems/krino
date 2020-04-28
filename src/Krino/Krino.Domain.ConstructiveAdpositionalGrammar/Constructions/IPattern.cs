using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;

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
        MorphemeRule MorphemeRule { get; }

        /// <summary>
        /// Rules for the matching adposition.
        /// </summary>
        PatternRule AdPositionRule { get; }

        /// <summary>
        /// Rules for the matching left child.
        /// </summary>
        PatternRule LeftRule { get; }

        /// <summary>
        /// Rules for the matching right child.
        /// </summary>
        PatternRule RightRule { get; }

        /// <summary>
        /// Rules for the matching governor.
        /// </summary>
        PatternRule GovernorRule { get; }

        /// <summary>
        /// Returns the valency position.
        /// </summary>
        int ValencyPosition { get; }
    }
}
