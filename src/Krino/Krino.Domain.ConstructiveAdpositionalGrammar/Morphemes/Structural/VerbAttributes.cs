using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Structural
{
    /// <summary>
    /// Verb attributes.
    /// </summary>
    public class VerbAttributes : EnumGroupBase
    {
        public VerbAttributes(EnumGroupBase parent) : base(parent)
        {
            Valency = new VerbValencyAttributes(this);
        }


        /// <summary>
        /// Valency of the verb.
        /// </summary>
        public VerbValencyAttributes Valency { get; }
    }
}
