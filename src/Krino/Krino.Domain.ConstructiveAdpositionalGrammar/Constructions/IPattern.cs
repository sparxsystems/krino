using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;

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
        /// Rules for the matching left child.
        /// </summary>
        PatternRule LeftRule { get; }

        /// <summary>
        /// Rules for the matching right child.
        /// </summary>
        PatternRule RightRule { get; }


        /// <summary>
        /// Returns the valency position.
        /// </summary>
        int ValencyPosition { get; }
    }
}
