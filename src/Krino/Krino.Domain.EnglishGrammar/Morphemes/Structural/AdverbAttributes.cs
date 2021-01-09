using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
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
    }
}
