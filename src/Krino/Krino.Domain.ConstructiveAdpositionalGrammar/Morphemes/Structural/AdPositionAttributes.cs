using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Structural
{
    /// <summary>
    /// AdPosition attributes.
    /// </summary>
    public class AdPositionAttributes : EnumGroupBase
    {
        public AdPositionAttributes(EnumGroupBase parent) : base(parent)
        {
            Lexeme = new EnumValue(this);
            NonLexeme = new AdPositionNonLexemeAttributes(this);
        }

        /// <summary>
        /// Lexeme.
        /// </summary>
        public EnumValue Lexeme { get; }

        /// <summary>
        /// Non-lexeme.
        /// </summary>
        public AdPositionNonLexemeAttributes NonLexeme { get; }
    }
}
