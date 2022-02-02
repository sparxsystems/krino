using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes.Sememes
{
    /// <summary>
    /// Voice attributes.
    /// </summary>
    public class VerbVoiceSememes : EnumGroupBase
    {
        public VerbVoiceSememes(EnumGroupBase parent) : base(parent)
        {
            Active = new EnumValue(this);
            Passive = new EnumValue(this);
        }

        /// <summary>
        /// Subject does the action.
        /// </summary>
        public EnumValue Active { get; }

        /// <summary>
        /// Subject receives the action.
        /// </summary>
        public EnumValue Passive { get; }
    }
}
