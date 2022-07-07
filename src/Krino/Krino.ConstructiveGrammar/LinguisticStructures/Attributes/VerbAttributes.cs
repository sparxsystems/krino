using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Verb attributes.
    /// </summary>
    public class VerbAttributes : EnumGroupBase
    {
        public VerbAttributes(EnumGroupBase parent) : base(parent)
        {
            Regular = new EnumValue(this);
            Irregular = new EnumValue(this);
            Stative = new StativeVerbAttributes(this);
            Action = new ActiveVerbAttributes(this);
            Auxiliary = new AuxiliaryVerbAttributes(this);
            InfinitiveMarker = new EnumValue(this);
            Form = new VerbFormAttributes(this);
            Valency = new VerbValencyAttributes(this);
        }


        /// <summary>
        /// Regular verb.
        /// </summary>
        public EnumValue Regular { get; }

        /// <summary>
        /// Irregular verb.
        /// </summary>
        public EnumValue Irregular { get; }

        /// <summary>
        /// Stative verb.
        /// </summary>
        /// <remarks>
        /// A stative verb defines the state of something: examples are sit, lie, stand.
        /// </remarks>
        public StativeVerbAttributes Stative { get; }

        /// <summary>
        /// An action verb is a verb that describes an action, like run, jump, kick, eat, break, cry, smile, or think. 
        /// </summary>
        public ActiveVerbAttributes Action { get; }

        /// <summary>
        /// Helper verbs e.g. will.
        /// </summary>
        public AuxiliaryVerbAttributes Auxiliary { get; }

        /// <summary>
        /// 'to' which marks the infinitive.
        /// </summary>
        public EnumValue InfinitiveMarker { get; }

        /// <summary>
        /// Form of the verb.
        /// </summary>
        public VerbFormAttributes Form { get; }

        /// <summary>
        /// Valency of the verb.
        /// </summary>
        public VerbValencyAttributes Valency { get; }
    }
}
