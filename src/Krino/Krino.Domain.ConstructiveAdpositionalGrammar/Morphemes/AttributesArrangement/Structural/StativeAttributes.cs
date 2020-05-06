using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    /// <summary>
    /// Stative attributes.
    /// </summary>
    public class StativeAttributes : EnumGroupBase
    {
        public StativeAttributes(EnumGroupBase parent) : base(parent)
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
