using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Structural
{
    public class VerbantLexemeAttributes : EnumGroupBase
    {
        public VerbantLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Verb = new VerbAttributes(this);
        }

        /// <summary>
        /// Verbant is a verb.
        /// </summary>
        public VerbAttributes Verb { get; }
    }
}
