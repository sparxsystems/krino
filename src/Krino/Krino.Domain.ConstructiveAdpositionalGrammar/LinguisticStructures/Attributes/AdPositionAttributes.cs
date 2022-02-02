using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// AdPosition attributes.
    /// </summary>
    public class AdPositionAttributes : EnumGroupBase
    {
        public AdPositionAttributes(EnumGroupBase parent) : base(parent)
        {
            Free = new AdPositionLexemeAttributes(this);
            Bound = new AdPositionNonLexemeAttributes(this);
        }

        /// <summary>
        /// Lexeme.
        /// </summary>
        public AdPositionLexemeAttributes Free { get; }

        /// <summary>
        /// Non-lexeme.
        /// </summary>
        public AdPositionNonLexemeAttributes Bound { get; }
    }
}
