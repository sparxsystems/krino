using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class AuxiliaryVerbAttributes : EnumGroupBase
    {
        public AuxiliaryVerbAttributes(EnumGroupBase parent) : base(parent)
        {
            Primary = new EnumValue(this);
            Modal = new EnumValue(this);
        }

        /// <summary>
        /// The primary auxiliary verbs are ‘be’, ‘have’ and ‘do’. These verbs modify other verbs in a full verb phrase, e.g. ‘is going’, ‘has gone’, or ‘did go’.
        /// </summary>
        public EnumValue Primary { get; }

        /// <summary>
        /// Modal auxiliary verbs like ‘can’ and ‘should’ usually occur with main verbs, e.g. ‘can pay’, ‘should pay’. They add meanings like possibility and obligation to the main verb.
        /// </summary>
        public EnumValue Modal { get; }
    }
}
