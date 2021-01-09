using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AttributesArrangement.Structural
{
    /// <summary>
    /// Attributes for stative lexemes.
    /// </summary>
    public class StativeLexemeAttributes : EnumGroupBase
    {
        public StativeLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Noun = new NounAttributes(this);
            Pronoun = new PronounAttributes(this);
        }

        /// <summary>
        /// The stative is a noun.
        /// </summary>
        public NounAttributes Noun { get; }

        /// <summary>
        /// The stative is a pronoun.
        /// </summary>
        public PronounAttributes Pronoun { get; }
    }
}
