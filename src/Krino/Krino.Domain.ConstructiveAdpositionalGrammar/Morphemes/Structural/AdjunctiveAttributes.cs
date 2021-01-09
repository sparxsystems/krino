using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Structural
{
    /// <summary>
    /// Adjunctive attributes.
    /// </summary>
    public class AdjunctiveAttributes : EnumGroupBase
    {
        public AdjunctiveAttributes(EnumGroupBase parent) : base(parent)
        {
            Lexeme = new EnumValue(this);
            NonLexeme = new AdjunctiveNonLexemeAttributes(this);
        }

        public EnumValue Lexeme { get; }

        public AdjunctiveNonLexemeAttributes NonLexeme { get; }
    }
}
