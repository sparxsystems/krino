using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
{
    /// <summary>
    /// Adjunctive attributes.
    /// </summary>
    public class AdjunctiveAttributes : EnumGroupBase
    {
        public AdjunctiveAttributes(EnumGroupBase parent) : base(parent)
        {
            Lexeme = new AdjunctiveLexemeAttributes(this);
            NonLexeme = new AdjunctiveNonLexemeAttributes(this);
        }

        public AdjunctiveLexemeAttributes Lexeme { get; }

        public AdjunctiveNonLexemeAttributes NonLexeme { get; }
    }
}
