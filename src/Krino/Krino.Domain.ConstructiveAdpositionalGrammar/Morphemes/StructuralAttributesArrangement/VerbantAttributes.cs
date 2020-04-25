using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement
{
    /// <summary>
    /// Verbant attributes.
    /// </summary>
    public class VerbantAttributes : EnumGroupBase
    {
        public VerbantAttributes(EnumGroupBase parent) : base(parent)
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
