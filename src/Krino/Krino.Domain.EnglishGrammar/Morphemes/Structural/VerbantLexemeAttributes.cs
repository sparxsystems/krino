using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
{
    public class VerbantLexemeAttributes : EnumGroupBase
    {
        public VerbantLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Verb = new VerbAttributes(this);
            Interjection = new EnumValue(this);
        }

        /// <summary>
        /// Verbant is a verb.
        /// </summary>
        public VerbAttributes Verb { get; }

        /// <summary>
        /// Verbant is an interjection.
        /// </summary>
        public EnumValue Interjection { get; }
    }
}
