using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Attributes
{
    /// <summary>
    /// Determiner (the word that introduces a noun) attributes.
    /// </summary>
    public class Determiner : EnumGroupBase
    {
        public Determiner(EnumGroupBase parent, int localPosition) : base(parent, 3, localPosition)
        {
            DefiniteArticle = new EnumValue(this, 1);
            IndefiniteArticle = new EnumValue(this, 2);
            Possessive = new EnumValue(this, 3);
        }

        /// <summary>
        /// E.g. the
        /// </summary>
        public EnumValue DefiniteArticle { get; }

        /// <summary>
        /// E.g. a, an
        /// </summary>
        public EnumValue IndefiniteArticle { get; }

        /// <summary>
        /// Indicates ownership of the noun e.g. my, his, your.
        /// </summary>
        public EnumValue Possessive { get; }
    }
}
