using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributes
{
    /// <summary>
    /// Determiner (the word that introduces a noun) attributes.
    /// </summary>
    public class Determiner : EnumGroupBase
    {
        public Determiner(EnumGroupBase parent) : base(parent)
        {
            DefiniteArticle = new EnumValue(this);
            IndefiniteArticle = new EnumValue(this);
            Possessive = new EnumValue(this);
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
