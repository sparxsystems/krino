using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Determiner (the word that introduces a noun) attributes.
    /// </summary>
    public class DeterminerAttributes : EnumGroupBase
    {
        public DeterminerAttributes(EnumGroupBase parent) : base(parent)
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
