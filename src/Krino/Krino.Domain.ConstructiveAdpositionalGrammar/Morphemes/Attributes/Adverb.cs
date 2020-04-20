using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Attributes
{
    /// <summary>
    /// Adveb attributes.
    /// </summary>
    public class Adverb : EnumGroupBase
    {
        public Adverb(EnumGroupBase parent, int localPosition) : base(parent, localPosition, 3)
        {
            Positive = new EnumValue(this, 1);
            Comparative = new EnumValue(this, 2);
            Superlative = new EnumValue(this, 3);
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
