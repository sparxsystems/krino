using Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes.Sememes;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Adveb attributes.
    /// </summary>
    public class AdverbAttributes : EnumGroupBase
    {
        public AdverbAttributes(EnumGroupBase parent) : base(parent)
        {
            Positive = new EnumValue(this);
            Comparative = new EnumValue(this);
            Superlative = new EnumValue(this);
            ConjunctiveAdverb = new EnumValue(this);

            Negation = new EnumValue(this);
            Affirmation = new EnumValue(this);

            Sememe = new AdverbSememes(this);
        }

        /// <summary>
        /// Default form of an adverb e.g. fast.
        /// </summary>
        public EnumValue Positive { get; }

        /// <summary>
        /// Form to compare two actions or states e.g. faster.
        /// </summary>
        public EnumValue Comparative { get; }

        /// <summary>
        /// Comparing one action or state with all the others in the same category e.g. fastest.
        /// </summary>
        public EnumValue Superlative { get; }

        /// <summary>
        /// Connects two independent clauses or sentences.
        /// </summary>
        /// <remarks>
        /// E.g.: also, consequently, furthermore, however, moreover, nevertheless, therefore.
        /// </remarks>
        public EnumValue ConjunctiveAdverb { get; }

        /// <summary>
        /// Adverbs of negation e.g. not, no.
        /// </summary>
        public EnumValue Negation { get; }

        /// <summary>
        /// Adverbs of affirmation e.g. yes, surely, ...
        /// </summary>
        public EnumValue Affirmation { get; }

        public AdverbSememes Sememe { get; }
    }
}
