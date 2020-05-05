using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.SemanticAttributesArrangement
{
    /// <summary>
    /// Voice attributes.
    /// </summary>
    public class VoiceAttributes : EnumGroupBase
    {
        public VoiceAttributes(EnumGroupBase parent) : base(parent)
        {
            Active = new EnumValue(this);
            Passive = new EnumValue(this);
        }

        /// <summary>
        /// Subject does the action.
        /// </summary>
        public EnumValue Active { get; }

        /// <summary>
        /// Sibject receives the action.
        /// </summary>
        public EnumValue Passive { get; }
    }
}
