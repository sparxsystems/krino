using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    /// <summary>
    /// Verbant attributes.
    /// </summary>
    public class VerbantAttributes : EnumGroupBase
    {
        public VerbantAttributes(EnumGroupBase parent) : base(parent)
        {
            Lexeme = new VerbantLexemeAttributes(this);
            NonLexeme = new VerbantNonLexemeAttributes(this);
        }

        public VerbantLexemeAttributes Lexeme { get; }

        public VerbantNonLexemeAttributes NonLexeme { get; }
    }
}
