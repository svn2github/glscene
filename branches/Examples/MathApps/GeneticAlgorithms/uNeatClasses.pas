unit uNeatClasses;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, System.Math,
  Vcl.Forms,
  uNEATStrFunctions, uNEATMisc;

// This is merely for testing purposes! It seems to reduce the quality of the
// result
{DEFINE USE_PRIORITY}

// Just for testing, see TConnect.SplitMutate for details
{DEFINE WEIGHT_FIRST}

// Should genotypes give their children a hint as to what species they belong
// too? Using hints is faster.
{$DEFINE REMEMBER_SPECIES}

// Makes sure that all outputnodes are sigmoids
{DEFINE ONLY_ALLOW_SIGMOID_IN_OUTPUT}

// Should ancestors be picked using fitness prop. selection, or simply randomly
// drawn from the species? Disabling this is typically bad!
{$DEFINE USE_FIT_PROP_IN_SPECIES}

// Classic NEAT don't count nodes when determining relatedness, but my version
// does. If you want classic neat, disable this.
{$DEFINE COUNT_NODES_IN_UNRELATEDNESS}

// Zero is min means that the network is trimmed to supply values in the range
// 0..1, if it's not defined, the range will be -1..1. Zero based seems to work
// better
{DEFINE ZERO_IS_MIN} // OFF => MY SIGMOID


const
  {$ifdef ZERO_IS_MIN}
    ZERO_BASED = true;
  {$else}
    ZERO_BASED = false;
  {$endif}


  CONNECTION_ENABLED_FLIP_CHANCE = 0.025;

  // This value is in relation to normal crossover - how many percent of ALL
  // crossovers should be averaging? Seems to give a _ tiny _ improvement, like
  // 4%, when ACE = 0.05
  AVERAGING_CROSSOVER_CHANCE = 0.4;

  ADD_CONNECTION_TRIES = 20;

  // In a population of a 150, 0.005 = 0.75 / generation
  NODE_FUNCTION_MUTATION_CHANCE = 0.005;

  PUNISH_SPECIES_AGE = 15;
  PUNISH_SPECIES_FACTOR = 1.0;

  STAGNATION_PERIOD = 14*10;
  STAGNATION_PUNISH_FACTOR = 200.0;

  MAJOR_STAGNATION_PERIOD = STAGNATION_PERIOD+1;

  MATE_ONLY_PROBABILITY = 0.2;

  MIGRATION_RATE = 0.001;
  REPRODUCE_MUTATE_RATE = 0.25;

  // Determines how much MinSpeciesRelatedness should change each generation
  MIN_SPECIES_RELATEDNESS_DELTA = 0.1;

  CONNECTION_INNOV_MEM = -1;

  // A high value here prevents nodes with the same ID from having different
  // functions within the same species
  cDifferentFunctionUnrelatednessWeight = 0.75;// 0.75;

  // A high value here prevents connections with the same ID from having different
  // delays within the same species
  cDifferentConnectionDelayUnrelatednessWeight = 0.75;

  // Use soreted or unsorted node/connection lists?
  cSortedNodeConnectLists = true; // This should be true
type
  TNode = class;
  TConnectList = class;
  TGenotype = class;
  TSpecies = class;
  TNEATPopulation=class;

  // TTransferFunction is the base class for handling transfer functions. Sigmoid
  // is the most common transferfunction in NNs.
  TTransferFunction=class
  private
    FTransferFunctionName: string;
    procedure SetTransferFunctionName(const Value: string);
  published
    // TransferFunctionName is the name of the transfer function
    property TransferFunctionName : string read FTransferFunctionName write SetTransferFunctionName;

    // This virtual method is used to get the value from the node, given it's
    // connections
    function CalculateValue(Node : TNode) : double;virtual;

    // GetSum will sum up all the connections values, multiplied with the
    // weights. This is used by many transfer functions
    function GetSum(Node : TNode) : double;

    // GetMul will multiply all the connections values, multiplied with the
    // weights. This is used by some transfer functions
    function GetMul(Node : TNode) : double;
  end;

  // TfrmFitnessMonitor is used to show the fitness progress of a population,
  // if the user so chooses. The base class TfrmFitnessMonitorBase is used
  // so that the TNEATPopulation can access certain important properties of the
  // population.
  TfrmFitnessMonitorBase = class(TForm)
  protected
    FBestFitness: double;
  public
    NEATPopulation : TNEATPopulation;
    RunSuccess : TList;
    TotalRuns : integer;
    TotalHits : integer;
    WinGeneration : integer;

    procedure SetBestFitness(const Value: double);virtual;
    property BestFitness : double read FBestFitness write SetBestFitness;
    procedure SetRunState(Running : boolean); virtual;
    procedure ResetForNewRun;virtual;
    procedure UpdatePopulationView;virtual;
    procedure PublishGenotype(Genotype: TGenotype);virtual;
  end;

  // TNode implements a neural node. Each neural node has a TransferFunction and
  // a list of connections. Using the TransferFunction the node can calculate
  // it's value given it's inputs.
  TNode = class
  private
    FValue: double;
    FIsOutputNode: boolean;
    FBiasNode: boolean;
    FIsInputNode: boolean;
    FOldValue: double;
    FActivationSum: double;
    FXPos: double;
    FYPos: double;
    FPriority: integer;
    FNodeID: integer;
    FActivationCount: integer;
    FEnabledConnectList: TConnectList;
    FTransferFunction: TTransferFunction;
    FGenotype: TGenotype;
    procedure SetValue(const Value: double);
    function GetValue: double;
  protected
    // TouchOfDeath is true if the node doesn't effect the network in any way.
    // When TouchOfDeath is set, there is a high chance that the node will be
    // deleted.
    FTouchOfDeath : boolean;
  public
    // ComingValue is used when values are delayed
    ComingValue : array[0..3] of double;

    // Copy creates a new node and then calls CopyTo, so that the new node is
    // a copy of the current node
    function Copy : TNode;

    // Copies all internal settings from this node to Node
    procedure CopyTo(Node : TNode);

    // Clear clears all internal storage, so it can be added to the cache and
    // then reused
    procedure Clear;

    // Clears all internal storage, preparing it for another iteration
    procedure Flush;

    // SaveToString creates a string that describes the node
    function SaveToString : string;

    // CalculateValue is responsible for calculating the value of this node,
    // it typically uses TransferFunction to do this.
    function CalculateValue : double;virtual;

    // MutateFunction sets a new (random) transfer function. The TNNTNEATPopulation
    // determines what transferfunction can be used.
    procedure MutateFunction;

    // Returns true if the node is valid, that is; it's -1<nodeid<NodeInnovationCounter
    function CheckNodeValidity : boolean;

    procedure ClearComingValue;

    constructor Create(Genotype : TGenotype);
    destructor Destroy;override;
  published
    // Value keeps track of the current value of the node
    property Value : double read GetValue write SetValue;

    // Determine in which order nodes should be evaluated, higher priority means
    // that nodes will be evaluated sooner. Output nodes alwas have the lowest
    // possible priority.
    property Priority : integer read FPriority write FPriority;

    // NodeID keeps track of the invention number for this node. NodeID is what
    // connections refer to.
    property NodeID : integer read FNodeID write FNodeID;

    // Genotype is the "parent" of the node. All nodes and connections are
    // organized into genotypes.
    property Genotype : TGenotype read FGenotype write FGenotype;

    // IsInputNode is true if the node is an input node. Inputnodes are treaded
    // differently, for insance, their value is never altered by the
    // transferfunction
    property IsInputNode : boolean read FIsInputNode write FIsInputNode;

    // IsOutputNode is true if the node is an output node.
    property IsOutputNode : boolean read FIsOutputNode write FIsOutputNode;

    // OldValue keeps track of the OLD value of the node. Some forms of network
    // relaxation uses OldValue, many types don't.
    property OldValue : double read FOldValue write FOldValue;

    // EnabledConnectList keeps track of the connections going into the node.
    // These connections are used when the node is evaluated, and only the
    // enabled connects are added.
    property EnabledConnectList : TConnectList read FEnabledConnectList;

    // Transferfunction is the function used to evaluate the node
    property TransferFunction : TTransferFunction read FTransferFunction write FTransferFunction;

    // Keeps track of how many times the node has been activated
    property ActivationCount : integer read FActivationCount write FActivationCount;

    // The sum of all the incoming values
    property ActivationSum : double read FActivationSum write FActivationSum;

    // YPos keeps track of how deep the node is. When a new node is created,
    // the from/to nodes are averaged for depth
    property YPos : double read FYPos write FYPos;

    // XPos isn't maintained by DelphiNEAT, but is set when rendering is done
    property XPos : double read FXPos write FXPos;

    // BiasNode should be set to true for bias nodes. This is used when
    // splitting connections
    property BiasNode : boolean read FBiasNode write FBiasNode;
  end;

  // TNodeList is simply a list of node
  TNodeList = class(TSortedList)
  private
    function GetItems(i: integer): TNode;
    procedure SetItems(i: integer; const Value: TNode);
  public
    function GetKeyForItem(Item : Pointer) : cardinal; override;
    property Items[i : integer] : TNode read GetItems write SetItems;default;
  end;

  // TConnect connects two nodes from Tail to Head. The connection "carries" the
  // value from the tail to the head, multiplying it with weight. 
  TConnect = class
  private
    FEnabled: boolean;
    FWeight: double;
    FHeadNodeID: integer;
    FInnovationID: integer;
    FTailNodeID: integer;
    FGenotype: TGenotype;
    FTailNode: TNode;
    FDelay: integer;
    FDelayedValueArray: array[0..5] of double;
    procedure SetDelay(const Value: integer);
  protected
    // TouchOfDeath is true if the connections doesn't effect the network in any
    // way. For instance, if the connections connects a node that isn't expressed
    // in the phenotype. When TouchOfDeath is set, there is a high chance that
    // the node will be deleted.
    FTouchOfDeath : boolean;
  public
    // Copy creates a new connection and then calls CopyTo, so that the new
    // connection is a copy of the current node
    function Copy : TConnect;

    // Copies all internal settings from this connection to connection
    procedure CopyTo(Connect : TConnect);

    // Clear clears all internal storage, so it can be added to the cache and
    // then reused
    procedure Clear;

    // FlipEnabled toggles the enabled boolean
    procedure FlipEnabled;

    // MutateSplit splits the connection into two connections, placing a new
    // node in the middle. The old connection is disabled. The first connection
    // to the new node is given a weight of 1. The connection from the new node
    // to the old head node is given the old weight.
    function MutateSplit : TNode;

    // MutateWeight perturbes the weight of the connection
    procedure MutateWeight;

    // Reverse swaps head with tail
    procedure Reverse;

    // Flush resets the value of the connect
    procedure Flush;

    // IdenticalTo determines whether a connect is indential to another connect
    // or not. It's used when determining if an innovation is "new" or not
    function IdenticalTo(Connect : TConnect) : boolean;

    // Returns true if the connect is valid, that is;
    // InnovationID is valid
    // TailNodeID is valid
    // HeadNodeID is valud
    function CheckConnectValidity : boolean;

    // IsRecurrent returns true if the node goes in backwards direction - from
    // output towards input
    function IsRecurrent : boolean;

    // Clear the array of delayed values for this connect
    procedure ClearDelayedValueArray;

    // Will calculate the value of this connection - it implements a delay
    // if it's been specifiec
    function GetValue : double;

    constructor Create(Genotype : TGenotype);
    destructor Destroy; override;

  published
    // InnovationID...
    property InnovationID : integer read FInnovationID write FInnovationID;

    // The node id of the node at the tail end of the connection
    property TailNodeID : integer read FTailNodeID write FTailNodeID;

    // The node id of the node at the head end of the connection
    property HeadNodeID : integer read FHeadNodeID write FHeadNodeID;

    // Weight a float value that is multiplied with the value of the tail node.
    property Weight : double read FWeight write FWeight;

    // Enabled determines if a connection should be expressed in phenotype at all,
    // or just remain in genotype. If enabled is false, the connection is not
    // expressed and the object doesn't influence the operations of the network
    property Enabled : boolean read FEnabled write FEnabled;

    // Genotype
    property Genotype : TGenotype read FGenotype;

    // TailNode is a direct reference to the node at tail, it's faster than
    // looking up TailNodeID every time.
    property TailNode : TNode read FTailNode;

    // Determines by how many activations the value passed through the connect
    // is delayed.
    property Delay : integer read FDelay write SetDelay;
  end;

  // TConnectList is simply a list of connections
  TConnectList = class(TSortedList)
  private
    function GetItems(i: integer): TConnect;
    procedure SetItems(i: integer; const Value: TConnect);
  public
    function GetKeyForItem(Item : Pointer) : cardinal; override;
    property Items[i : integer] : TConnect read GetItems write SetItems;default;
  end;

  // TGenotype organizes a lot of connections and nodes into one genotype. A
  // genotype can in turn have a fitness and can be evaluated to determine this
  // fitness. The genotype can also create the phenotype of the neural net,
  // though this is done implicitly within the class
  TGenotype = class
  private
    FPhenotypePrepared: boolean;
    FFitness: double;
    FAdjustedFitness: double;
    FMaxNodeDepth: integer;
    FTag: integer;
    FNextInputNode: integer;
    FExpectedOffspring: integer;
    FConnectList: TConnectList;
    FNEATPopulation: TNEATPopulation;
    FInputNodes: TNodeList;
    FNodeEvaluationList: TNodeList;
    FOutputNodes: TNodeList;
    FNodeList: TNodeList;
    FSpeciesHint: TSpecies;
    function GetInputs(i: integer): double;
    function GetOutputs(i: integer): double;
    procedure SetInputs(i: integer; const Value: double);
    procedure SetOutputs(i: integer; const Value: double);
  public
    // LoadValues is a method that takes an array of doubles and sets the
    // values inputnodes to the supplied values
    procedure LoadValues(a : array of double);

    // LoadFromFile loads a genotype with nodes and connects from a file.
    // Extensions named .KNEAT are loaded as Ken's original output
    // Extensions named .DNEAT are loaded as DelphiNEAT output
    procedure LoadFromFile(FileName : string);

    // LoadDNEATFromStrings and LoadKNEATFromStrings generate genotypes from a
    // description provided in a TStrings variable.
    procedure LoadDNEATFromStrings(Strings : TStrings);
    procedure LoadKNEATFromStrings(Strings : TStrings);

    // GetSize returns the number of genes in the genotype
    function GetSize : integer;

    // GetHiddenNodeCount returns the number of hidden nodes in the genotype
    function GetHiddenNodeCount : integer;

    // AddNewNode inserts a new node, giving it a new innovation#
    function AddNewNode(NodeID : integer) : integer;

    // AddConnection will add a new connection from tailnodeid to headnodeid,
    // using the weight weight
    function AddConnection(TailNodeID, HeadNodeID: integer;
      Weight: double) : TConnect;

    // PhenotypePrepared indicates whether the genotype has been expressed into
    // phenotype yet. Before PhenotypePrepared is set, iterate can't be called!
    property PhenotypePrepared : boolean read FPhenotypePrepared;

    // Copy creates a new genotype and calls copyto
    function Copy : TGenotype;

    // Cross mates two parents creating a child
    procedure Cross(Mother, Father : TGenotype);

    // CopyTo copies all the nodes and connections to Genotype
    procedure CopyTo(Genotype : TGenotype);

    // Flush reset the Value field of all nodes in the genotype/phenotype
    procedure Flush;
    procedure PrepareInputs;

    // Iterate updates all the values of the nodes in the genotype CNT times.
    // if cnt=-1, then NEATPopulation.ActivationIterations will be used
    // NEATPopulation.ActivationIterations = 0, then MaxNodeDepth will be used
    // if NEATPopulation.ActivationIterations = -1, then the network will iterate
    // until it stabelizes.
    procedure Iterate(cnt : integer = -1);

    // Stablized checks all nodes in the genotype to see if they're stablized
    function Stabilized : boolean;

    // GetRandomConnect picks out a random connection out of the list of
    // connections. This is used for mutations
    function GetRandomConnect : TConnect;
    function GetRandomActiveConnect : TConnect;

    // GetRandomNode picks out a random node out of the list of
    // nodes. This is used for mutations.
    function GetRandomNode(ForbidInput : boolean) : TNode;

    // GetNodeByID locates a node that matches NodeId. If no node is found NIL
    // will be returned.
    function GetNodeByID(NodeId : integer) : TNode;

    // GetConnectByID locates a connect that matches ID. If no node is found NIL
    // will be returned.
    function GetConnectByID(Id : integer) : TConnect;

    // Mutate will mutate both nodes and connections
    procedure Mutate;

    // RemoveConnection will free a connect and remove it from the connectlist
    procedure RemoveConnection(Connect : TConnect);

    // AddRandomConnection creates a new random connection between two nodes in
    // the genotype
    procedure AddRandomConnection;
    procedure AddRandomConnections(const ACount : integer);
    procedure DisableRandomConnections(const ACount : integer);
    procedure DeleteRandomConnections(const ACount : integer);

    // ConnectionExists takes a tailnodeid and a headnodeid and checks all
    // connections to see if such a connection allready exists
    function ConnectionExists(TailNodeID, HeadNodeID : integer) : boolean;

    // PreparePhenotype updates all nodes and connections, making sure they're
    // ready for iterations.
    procedure PreparePhenotype;

    // SaveToString creates a readable string of the genotype
    function SaveToString : string;

    // AsCode generates delphi code of the genotype in question
    function AsCode : string;

    // SetupInitialGenotype creates a fully connected genotype with the given
    // number of inputs/outputs. This is used to seed the TNEATPopulation
    procedure SetupInitialGenotype(Inputs, Outputs : integer);

    // CacheNodesAndConnects puts all nodes and connects in the cache
    procedure CacheNodesAndConnects;

    // UnRelatedness compares two genomes, based on InnovationID and NodeID and
    // returns a value determening how closesly related the two are
    function Unrelatedness(Genotype : TGenotype) : double;

    // Checks if two genotypes are identical
    function Identical(Genotype : TGenotype) : boolean;

    // OutputsOff goes through all the outputs and makes sure that they're all on
    function OutputsOff : boolean;

    function SetNextInputValue(const AValue : double) : integer;

    constructor Create(ANEATPopulation : TNEATPopulation);
    destructor Destroy;override;

    // Inputs is a quick version to access InputNodes[i].Value
    property Inputs[i : integer] : double read GetInputs write SetInputs;

    // Outputs is a quick version to access OutputNodes[i].Value
    property Outputs[i : integer] : double read GetOutputs write SetOutputs;

  published
    // NodeList is a list of all nodes in the genotype
    property NodeList : TNodeList read FNodeList;

    // NodeEvaluationList is the nodes of the genotype, ordered accordning to
    // their priority
    property NodeEvaluationList : TNodeList read FNodeEvaluationList;

    // ConnectList is a list of all connections in the genotype
    property ConnectList : TConnectList read FConnectList;

    // InputNodes is a quick reference to the input nodes of the genotype
    property InputNodes : TNodeList read FInputNodes;

    // OutputNodes is a quick reference to the output nodes of the genotype
    property OutputNodes : TNodeList read FOutputNodes;

    // TNEATPopulation keeps track of the TNEATPopulation that this genotype belongs to
    property NEATPopulation : TNEATPopulation read FNEATPopulation;

    // Fitness is a value describing how fit the genotype is. Fitness must ALWAYS
    // be above or equal to zero. Higher fitness means more fit individual. Thus,
    // 0 is the worst possible fitness.
    property Fitness : double read FFitness write FFitness;

    // AdjustedFitness is the fitness after it has been altered by fitness
    // sharing, penalties and such
    property AdjustedFitness : double read FAdjustedFitness write FAdjustedFitness;

    // Tag is a storage that the application developers can use as they see fit
    property Tag : integer read FTag write FTag;

    // The number of offspring that this genotype expects
    property ExpectedOffspring : integer read FExpectedOffspring;

    // MaxNodeDepth keeps track of how deep an individual is, from input to
    // output. The ONLY place a MaxNodeDepth can increase, is in connection
    // splits. Adding connections, mutating things, these will never alter
    // the depth. If they do, there's a bug.
    property MaxNodeDepth : integer read FMaxNodeDepth write FMaxNodeDepth;

    // SpeciesHint is the species of the mother of the genotype. A genotype is
    // likely to belong to the same species as it's mother, so this variable
    // is used when placing the genotype in a species.
    property SpeciesHint : TSpecies read FSpeciesHint write FSpeciesHint;
  end;

  // TGenotypeList is simply a list of TGenotypes
  TGenotypeList = class(TList)
  private
    function GetItems(i: integer): TGenotype;
    procedure SetItems(i: integer; const Value: TGenotype);
  public
    property Items[i : integer] : TGenotype read GetItems write SetItems;default;
  end;


  // TSpecies organize species from one generation to the next.
  TSpecies = class
  private
    // The NEATPopulation that the species belongs to.
    FNEATPopulation : TNEATPopulation;
    FDeadSpecies: boolean;
    FFitnessSum: double;
    FAdjustedFitnessSum: double;
    FSpeciesPunish: double;
    FOldChampionFitness: double;
    FBirthGeneration: integer;
    FMemberCount: integer;
    FSpeciesID: integer;
    FColor: integer;
    FExpectedOffspringCount: integer;
    FLastImprovementGeneration: integer;
    FDefiningMember: TGenotype;
    FChampion: TGenotype;
    FGenotypes: TGenotypeList;
  public
    // Calculates how many offspring the species should have
    procedure UpdateExpectedOffspringCount;

    // WeedOutDeadOnes will clear out all individuals that have too low fitness
    // to be allowed to have offspring. ( determined by SURVIVAL_THRESHOLD )
    procedure WeedOutDeadOnes;

    // CalculateAdjustedFitnessSum will calculate and set AdjustedFitnessSum
    procedure CalculateAdjustedFitnessSum;

    // FitnessPropSelection will pick an individual based on it's fitness
    function FitnessPropSelection : TGenotype;

    // TournamentSelection will test a number of genotypes (GenotypesToTry) and
    // return the most fit
    function TournamentSelection(GenotypesToTry : integer) : TGenotype;

    // GetRandomGenotype returns a random genotype from the species
    function GetRandomGenotype : TGenotype;

    procedure Add(Genotype : TGenotype);

    constructor Create(ANEATPopulation : TNEATPopulation);
    destructor Destroy;override;
  published
    // DefiningMember is the genotype that is used as a yard-stick to determine
    // is an individual belongs in the secies or not. If the unrelatedness
    // between a genotype and the DefiningMember is too big, it's not considered
    // to belong to the species. The DefiningMember belongs to the species, and
    // can be destroyed as the species sees fit.
    property DefiningMember : TGenotype read FDefiningMember;

    // MemberCount keeps track of how many members were sorted into the species
    property MemberCount : integer read FMemberCount;

    // Genotypes is a list of all the genotypes that are in the species
    property Genotypes : TGenotypeList read FGenotypes;

    // Champion is the champion of a species, the genotype with the highest
    // fitness
    property Champion : TGenotype read FChampion;

    // OldChampionFitness keeps track of the fitness of the old champion - it's
    // used to track improvements in the species.
    property OldChampionFitness : double read FOldChampionFitness;

    // FitnessSum is the sum of all the fitnesses in the species.
    property FitnessSum : double read FFitnessSum;

    // AdjustedFitnessSum is the sum of all the adjusted fitnesses in the species.
    property AdjustedFitnessSum : double read FAdjustedFitnessSum;

    // BirthGeneration is set to the generation that the species is created. This
    // can be used to punish old species
    property BirthGeneration : integer read FBirthGeneration;

    // LastImprovementGeneration keeps track of the last time the species
    // improved, this is used to punish stagnant species
    property LastImprovementGeneration : integer read FLastImprovementGeneration;

    // SpeciesID is a counter that keeps track of each species that's created
    // during a run.
    property SpeciesID : integer read FSpeciesID;

    // A dead species cannot contain genotypes! But species are kept around,
    // even though they're dead, to enable statistics collecting.
    property DeadSpecies : boolean read FDeadSpecies;

    // ExpectedOffspringCount determines how many offspring this species is
    // expecting for the next generation
    property ExpectedOffspringCount : integer read FExpectedOffspringCount;

    // SpeciesPunish is a value that determines how much the fitness of
    // individuals in the species should be punished due to age and stagnancy.
    property SpeciesPunish : double read FSpeciesPunish;

    // This is used for visualization purposes
    property Color : integer read FColor;
  end;

  // TSpeciesList is simply a list of TSpecies
  TSpeciesList = class(TList)
  private
    function GetItems(i: integer): TSpecies;
    procedure SetItems(i: integer; const Value: TSpecies);
  public
    property Items[i : integer] : TSpecies read GetItems write SetItems;default;
  end;

  TIterationState = (isPreIterations, isMidIterations, isPostIterations);
  TSelectionType = (stFitProp, stSpecies, stTournament, stLocalTournament);
  TSpeciesFitnessMethod = (sfmAverageFitness, sfmHighestFitness);

  TOnGenotypeIteration = procedure (NEATPopulation : TNEATPopulation; Genotype : TGenotype; IterationState : TIterationState) of object;
  TOnPrepareInitialGenotype = procedure (NEATPopulation : TNEATPopulation; Genotype : TGenotype) of object;
  TOnCalculateFitness = function (NEATPopulation : TNEATPopulation; Genotype : TGenotype) : double of object;
  TOnShowBestIndividual = function (NEATPopulation : TNEATPopulation; Genotype : TGenotype; DifferentFromPrevious : boolean) : double of object;

  // TNEATPopulation is a class that keeps track of a large number of TGenotype,
  // and basically takes care of the evolution.
  TNEATPopulation = class(TComponent)
  private
    // FGenerationInnovationList keeps track of the innovation that have been
    // created this generation. Used to avoid identical innovations getting
    // different innovation numbers.
    FGenerationInnovationList : TConnectList;

    // *** MISC
    FOnCalculateFitness: TOnCalculateFitness;
    FMinSpeciesRelatedness: double;
    FSelectionType: TSelectionType;
    FOutputNodeCount: integer;
    FInputNodeCount: integer;
    FPopulationSize: integer;
    FReproduceMutateRate: double;
    FSpeciesTargetCount: integer;
    FGenerationsToRun: integer;
    FStopped : boolean;
    FTargetFitness: double;
    FOnShowBestIndividual: TOnShowBestIndividual;
    FBestFitnessOldGeneration : double;
    FCurrentlyRunning: boolean;
    FUseFitnessMonitor: boolean;
    FFitnessMonitor: TfrmFitnessMonitorBase;
    FOnBeforeStartRun: TNotifyEvent;
    FOnAfterRunStopped: TNotifyEvent;
    FTournamentRange: integer;
    FTournamentSize: integer;
    FConnectionWeightMutationChance: double;
    FConnectionMutationBigChangeChance: double;
    FMutateCrossed: boolean;
    FFailedIterations : integer;
    FConnectNodesOfInitialPop: boolean;
    FConnectOutputOfInitialPop: boolean;
    FActivationIterations: integer;
    FAllowRecurrentLinks: boolean;
    FLivingSpeciesCount: integer;
    FSurvivalThreshold: double;
    FSpeciesFitnessMethod: TSpeciesFitnessMethod;
    FOnPrepareInitialGenotype: TOnPrepareInitialGenotype;
    FOnGenotypeIteration: TOnGenotypeIteration;
    FGenotypeIterationEvent: boolean;
    FConnectionAddChance: double;
    FConnectionSplitChance: double;
    FWeightUnrelatednessFactor: single;
    FWeightPeturbationFactor: single;
    FInitialWeightMagnitude: single;
    FChampionAdjustedFitnessSum: double;
    FFitnessSum: double;
    FAdjustedFitnessSum: double;
    FChampionFitnessSum: double;
    FIndividualsEvaluatedThisRun: integer;
    FConnectInnovationCounter: integer;
    FNodeInnovationCounter: integer;
    FCurrentGeneration: integer;
    FBestGenotype: TGenotype;
    FGenotypes: TGenotypeList;
    FOldGeneration: TGenotypeList;
    FSpeciesList: TSpeciesList;
    FMaxConnectionDelay: integer;
    FMaxLinkWeight: single;
    FCurrentGenotypeID: integer;
    procedure SetOnCalculateFitness(const Value: TOnCalculateFitness);
    procedure SetMinSpeciesRelatedness(const Value: double);
    procedure SetSelectionType(const Value: TSelectionType);
    procedure SetInputNodeCount(const Value: integer);
    procedure SetOutputNodeCount(const Value: integer);
    procedure SetPopulationSize(const Value: integer);
    procedure SetReproduceMutateRate(const Value: double);
    procedure SetSpeciesTargetCount(const Value: integer);
    procedure SetGenerationsToRun(const Value: integer);
    procedure SetTargetFitness(const Value: double);
    procedure SetOnShowBestIndividual(const Value: TOnShowBestIndividual);
    procedure SetUseFitnessMonitor(const Value: boolean);
    procedure SetFitnessMonitor(const Value: TfrmFitnessMonitorBase);

    // DoOneRun will run ONE population until it wins or runs out of generations
    procedure DoOneRun;
    procedure SetOnBeforeStartRun(const Value: TNotifyEvent);
    procedure SetOnAfterRunStopped(const Value: TNotifyEvent);
    procedure SetTournamentRange(const Value: integer);
    procedure SetTournamentSize(const Value: integer);
    procedure SetConnectionWeightMutationChance(const Value: double);
    procedure SetConnectionMutationBigChangeChance(const Value: double);
    procedure SetMutateCrossed(const Value: boolean);
    procedure SetConnectNodesOfInitialPop(const Value: boolean);
    procedure SetConnectOutputOfInitialPop(const Value: boolean);
    procedure SetActivationIterations(const Value: integer);
    procedure SetAllowRecurrentLinks(const Value: boolean);
    procedure SetSurvivalThreshold(const Value: double);
    procedure SetSpeciesFitnessMethod(const Value: TSpeciesFitnessMethod);
    procedure SetOnPrepareInitialGenotype(
      const Value: TOnPrepareInitialGenotype);
    procedure SetOnGenotypeIteration(const Value: TOnGenotypeIteration);
    procedure SetGenotypeIterationEvent(const Value: boolean);
    procedure SetBestFitnessOldGeneration(const Value: double);
    procedure SetConnectionAddChance(const Value: double);
    procedure SetConnectionSplitChance(const Value: double);
    procedure SetWeightUnrelatednessFactor(const Value: single);
    procedure SetWeightPeturbationFactor(const Value: single);
    procedure SetInitialWeightMagnitude(const Value: single);
  protected
    // FSplitInnovationList keeps track of the split innovations that took place
    // this generation
    FSplitInnovationList : TList;

    // SpeciesIDCounter is used to set SpeciesID on species.
    FSpeciesIDCounter : integer;

    // ClearSplitInnovations frees all split innovations and clears the list
    procedure ClearSplitInnovations;

    // Notofication is used to update the component when another component is
    // destroyed or added.
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    // Start the evolution if autostart is set and Loaded is complete
    procedure Loaded;override;
  public
    // StartRun will start running the population, generation by generation, and
    // restart the entire run when either a winner is found or the current generation
    // exceeds GenerationsToRun.
    procedure StartRun(Runs : integer=-1; ResetFitnessMonitor : boolean=true);

    // StopRun will terminate the run
    procedure StopRun;

    procedure DeleteAllConnectInnovations;

    // ShowBestIndividual will fire the FOnShowBestIndividual, if it's assigned
    procedure ShowBestIndividual;

    // WinnerFound returns true when the best genome has a fitness higher than
    // the fitness target
    function WinnerFound : boolean;

    // GetNextConnectInnovation is used when creating new innovations, it
    // makes sure that there aren't any copies of the new innovation. If there
    // are, it simply returns that innovation number. If there aren't, it creates
    // a new innovation number
    function GetNextConnectInnovation(Connect : TConnect) : integer;

    // GetNextNodeInnovation is used when creating new innovations, it
    // makes sure that there aren't any copies of the new innovation. If there
    // are, it simply returns that innovation number. If there aren't, it creates
    // a new innovation number
    function GetNextNodeInnovation(Node : TNode) : integer;

    // SwapPopulations swaps between the old generation and the new one, this
    // is done in preparation of creating a new generation (the current
    // generation then becomes old).
    procedure SwapPopulations;

    // CreateNewGeneration creates a completely new generation based on the fitness
    // of the old generation (it also handles the swap).
    procedure CreateNewGeneration;

    // CreateNewSpeciesBasedGeneration is used by CreateNewGeneration if the
    // selection type is stSpecies. It creates a new generation based on the
    // species fitnesses.
    procedure CreateNewSpeciesBasedGeneration;

    // CreatePopulation creates a new initial population
    procedure CreatePopulation;

    // ClearPopulation deletes all genomes in the current population
    procedure ClearPopulation;

    // CalculateFitnesses will cycle through all genomes in the current population,
    // and set their fitnesses using the OnCalculateFitness event.
    // A OnCalculateFitness MUST be provided
    procedure CalculateFitnesses;virtual;

    // PickAncestor picks an ancestor from the old generation, however, there
    // are several ways of picking ancestors.
    function PickAncestor(i : integer; AvoidGenotype : TGenotype) : TGenotype;

    // PickAncestorFromSpecies picks ancestors that belong to the same species
    // as the AvoidGenotype 
    function PickAncestorFromSpecies(AvoidGenotype : TGenotype) : TGenotype;

    // Selects a random species ackording to fitness
    function PickSpeciesFitnessProp : TSpecies;

    // PickAncestorLinear picks an ancestor that lies close to i in the array
    // of genotypes. This makes migration slow
    function PickAncestorLinear(i : integer; AvoidGenotype : TGenotype) : TGenotype;

    // PickAncestorTournament goes through the list of genotypes and picks the
    // the best one it encounters, trying TournamentSize times.
    function PickAncestorTournament(GenotypeList : TGenotypeList; TournamentSize : integer; UseSpeciesFitness : boolean; AvoidGenotype : TGenotype=nil) : TGenotype;

    // PickAncestorFitProp picks genotypes proportionate to their fitness
    function PickAncestorFitProp(GenotypeList : TGenotypeList; AdjustedFitnessSum : double) : TGenotype;

    // TournamentSelectSpecies will try N species and pick the one with the
    // highest fitness
    function TournamentSelectSpecies(SpeciesCount : integer) : TSpecies;

    // GetRandomTransferFunction returns a random transfer function that's
    // allowed in the genome
    function GetRandomTransferFunction : TTransferFunction;

    // Free all the TGenomeLists in the specieslist, and clear the list
    procedure ClearSpeciesList(FreeAll : boolean=false);

    // PlaceInSpecies goes through all the genomes and places them in species
    // depending on relatedness
    procedure PlaceInSpecies;

    // AdjustMinSpeciesRelatedness will update the value of
    // MinSpeciesRelatedness and try to maintain the requested number
    // (SpeciesTargetCount) of species in the popuiation
    procedure AdjustMinSpeciesRelatedness;

    // GetRandomWeight returns a random weight
    function GetRandomWeight : double;

    // GetAverageSize returns the average size of the genotypes in the population
    function GetAverageSize : double;

    // GetAverageFitness returns the average fitness of the genotypes in the pop.
    function GetAverageFitness : double;

    // GetMinWeight returns -1, it has been deprecated
    function GetMinWeight : double;

    // CreateFitnessMonitorIfWanted creates a fitness monitor if the value for
    // UseFitnessMonitor is true
    procedure CreateFitnessMonitor;

    // Returns a random delay for new connects
    function GetRandomConnectionDelay : integer;

    // GetSpeciesForGenotype searches through the species and picks the FIRST
    // one where the genotype fits. If it doesn't fit in any of them, nil will
    // be returned.
    function GetSpeciesForGenotype(Genotype : TGenotype) : TSpecies;

    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
  public
    // ***** PUBLIC PROPERTIES

    // Genotypes is a list of all genotypes in the CURRENT population
    property Genotypes : TGenotypeList read FGenotypes;

    // OldGeneration is a list of all genotypes in the OLD population. It's used
    // when creating the new generation for mating and such purposes.
    property OldGeneration : TGenotypeList read FOldGeneration;

    // BestGenotype is the best genotype of the latest fitness calculation
    property BestGenotype : TGenotype read FBestGenotype;

    // FitnessSum is the sum of all fitnesses this generation
    property FitnessSum : double read FFitnessSum;

    // AdjustedFitnessSum is the sum of all adjusted fitnesses this generation
    property AdjustedFitnessSum : double read FAdjustedFitnessSum;

    // ChampionAdjustedFitnessSum is the sum of all species champions adjusted
    // fitness
    property ChampionAdjustedFitnessSum : double read FChampionAdjustedFitnessSum;

    // ChampionFitnessSum is the sum of all species champions fitness
    property ChampionFitnessSum : double read FChampionFitnessSum;

    // NodeInnovationCounter keeps track of how many NODE inovations have been
    // created so far, and is also used to make sure that all new innovations
    // get unique numbers.
    property NodeInnovationCounter : integer read FNodeInnovationCounter write FNodeInnovationCounter;

    // ConnectInnovationCounter keeps track of how many CONNECTION inovations
    // have been created so far, and is also used to make sure that all new
    // innovations get unique numbers.
    property ConnectInnovationCounter : integer read FConnectInnovationCounter write FConnectInnovationCounter;

    // SpeciesList is a list that keep track of the different species in the
    // population. Each species is represented by a TSpecies. Genomes are
    // placed in species depending on their relatedness.
    property SpeciesList : TSpeciesList read FSpeciesList;

    // CurrentGeneration determines what generation is currently being evaluated
    property CurrentGeneration : integer read FCurrentGeneration;

    // IndividualsEvaluatedThisRun keeps track of how many individuals have been
    // evaluated this run
    property IndividualsEvaluatedThisRun : integer read FIndividualsEvaluatedThisRun;

    // Keeps track of the fitness of the previous generation
    property BestFitnessOldGeneration : double read FBestFitnessOldGeneration write SetBestFitnessOldGeneration;

    // Use CurrentlyRunning to determine if the run is on or not
    property CurrentlyRunning : boolean read FCurrentlyRunning;

    // FailedIterations indicates how many iterations failed to complete and
    // were thus aborted.
    property FailedIterations : integer read FFailedIterations;

    // FitnessMonitor is a form that shows the fitness progress of a run.
    // Application developers can access the fitnessmonitor through this prop.
    property FitnessMonitor : TfrmFitnessMonitorBase read FFitnessMonitor write SetFitnessMonitor;

    // LivingSpeciesCount returns the number of species that are currently alive
    property LivingSpeciesCount : integer read FLivingSpeciesCount;

    property CurrentGenotypeID : integer read FCurrentGenotypeID; 
  published
    // ***** PUBLISHED PROPERTIES

    // PopulationSize determines how large the population should be, that is,
    // how many genotypes should be in it.
    property PopulationSize : integer read FPopulationSize write SetPopulationSize default 150;

    // SpeciesTargetCount, when set to something other than zero, will make the
    // population alter the MinSpeciesRelatedness number and try to main a
    // number of species that's close to SpeciesTargetCount.
    property SpeciesTargetCount : integer read FSpeciesTargetCount write SetSpeciesTargetCount;

    // ReproduceMutateRate determines how large a portion of the population
    // should be reproduced and mutated instead of crossed
    property ReproduceMutateRate : double read FReproduceMutateRate write SetReproduceMutateRate;

    // MinSpeciesRelatedness determines how related two genotypes must be to be
    // considered of one species.
    property MinSpeciesRelatedness : double read FMinSpeciesRelatedness write SetMinSpeciesRelatedness;

    // Selection type determines how ancestors will be selected for mating
    property SelectionType : TSelectionType read FSelectionType write SetSelectionType default stSpecies;

    // InputNodeCount determines how many input nodes will be created for the
    // genotypes in the population
    property InputNodeCount : integer read FInputNodeCount write SetInputNodeCount default 1;

    // OutputNodeCount determines how many output nodes will be created for the
    // genotypes in the population
    property OutputNodeCount : integer read FOutputNodeCount write SetOutputNodeCount default 1;

    // GenerationsToRun determine how many generations each run should be,
    // though if the run fins a winner it will not run all generations
    property GenerationsToRun : integer read FGenerationsToRun write SetGenerationsToRun default 65;

    // When a genotype has a fitness that's >=TargetFitness, then the genotype
    // is considered a winner
    property TargetFitness : double read FTargetFitness write SetTargetFitness;

    // UseFitnessMonitor determines if a fitness monitor should be auto-created
    // or not.
    property UseFitnessMonitor : boolean read FUseFitnessMonitor write SetUseFitnessMonitor default true;

    // TournamentSize determines the number of genotypes to include in a
    // tournament, if either stTournament or stLocalTournament is used for
    // selection type.
    property TournamentSize : integer read FTournamentSize write SetTournamentSize default 7;

    // TournamentRange determines the range from which genotypes will be selected
    // if stLocalTournament is used for selection type. In stLocalTournament, a
    // number of genotypes ( TournamentSize ) are tested, and they must all have
    // an index in the Genotypes list that is i+- TournamentRange.
    property TournamentRange : integer read FTournamentRange write SetTournamentRange default 14;

    // ConnectionWeightMutationChance determines how large the chance is for each
    // connection weight to be mutated
    property ConnectionWeightMutationChance : double read FConnectionWeightMutationChance write SetConnectionWeightMutationChance;

    // ConnectionMutationBigChangeChance determines how big the chance of a
    // complete replace of a connection weight is, and how big the chance of a
    // smaller change is.
    property ConnectionMutationBigChangeChance : double read FConnectionMutationBigChangeChance write SetConnectionMutationBigChangeChance;

    // MutateCrossed determines whether children of crossover should be mutated
    // as well
    property MutateCrossed : boolean read FMutateCrossed write SetMutateCrossed default true;

    // ConnectNodesOfInitialPop determines if a minimal network should be
    // created when a new run is started. This should typically be set to true
    property ConnectNodesOfInitialPop : boolean read FConnectNodesOfInitialPop write SetConnectNodesOfInitialPop default True;

    // ConnectOutputOfInitialPop determines if cyclic connections should be
    // created when a new run is started. This should typically be set to false
    property ConnectOutputOfInitialPop : boolean read FConnectOutputOfInitialPop write SetConnectOutputOfInitialPop default False;

    // ActivationIterations determines how many iterations an Genotype should
    // perform when Iterate is called. The default number of iterations is 3.
    property ActivationIterations : integer read FActivationIterations write SetActivationIterations default 1;

    // AllowRecurrentLinks determines if links are allowed to go upstream in
    // the network. If set to false, connections may not go from a node to a
    // node with lower depth.
    property AllowRecurrentLinks : boolean read FAllowRecurrentLinks write SetAllowRecurrentLinks default True;

    // SurvivalThreshold determines how many of a species gets to survive and
    // act as parents. The lower this value, the greedier the strategy.
    property SurvivalThreshold : double read FSurvivalThreshold write SetSurvivalThreshold;

    // Should max fitness or average fitness determine how many offspring a species
    // has? I'm not sure which version is better - they may be better at different
    // things.
    property SpeciesFitnessMethod : TSpeciesFitnessMethod read FSpeciesFitnessMethod write SetSpeciesFitnessMethod default sfmAverageFitness;

    // If GenotypeIterationEvent is true, the event OnGenotypeIteration will
    // be fired each iteration of a genotype. This would typically be set to true
    // in ShowBestIndividual.
    property GenotypeIterationEvent : boolean read FGenotypeIterationEvent write SetGenotypeIterationEvent default False;

    // How often should a genotype have one of it's connections split, expressed
    // as a fraction. Default is 0.03
    property ConnectionSplitChance : double read FConnectionSplitChance write SetConnectionSplitChance;

    // How often should a random connection be added to a genotype, expressed as
    // a fraction. Default is 0.05
    property ConnectionAddChance : double read FConnectionAddChance write SetConnectionAddChance;

    // How much weight differences will influence unrelatedness - used in
    // unrelatedness calculations.
    property WeightUnrelatednessFactor : single read FWeightUnrelatednessFactor write SetWeightUnrelatednessFactor;

    // WeightPeturbationFactor determine how much a connection weight can mutate
    // in each mutation.
    property WeightPeturbationFactor : single read FWeightPeturbationFactor write SetWeightPeturbationFactor;

    // InitialWeightMagnitude determines how large connection weghts should be
    // when they're randomly created
    property InitialWeightMagnitude : single read FInitialWeightMagnitude write SetInitialWeightMagnitude;

    // MaxConnectionDelay determines how much the input/output of a connect is
    // allowed to be delayed. Traditionally, this value is 0 for NNs.
    property MaxConnectionDelay : integer read FMaxConnectionDelay write FMaxConnectionDelay default 0;

    property MaxLinkWeight : single read FMaxLinkWeight write FMaxLinkWeight;

    // ***** EVENTS

    // OnCalculateFitness is the most important property of the population, it
    // determines how an individual can be rated for fitness.
    property OnCalculateFitness : TOnCalculateFitness read FOnCalculateFitness write SetOnCalculateFitness;

    // OnShowBestIndividual is fired at the end of every generation, giving the
    // application developer the chance to do something with the best individual
    // of that generation. Typically, it is used to show the progress of the
    // run.
    property OnShowBestIndividual : TOnShowBestIndividual read FOnShowBestIndividual write SetOnShowBestIndividual;

    // OnBeforeStartRun is fired before a run is started
    property OnBeforeStartRun : TNotifyEvent read FOnBeforeStartRun write SetOnBeforeStartRun;

    // OnAfterRunStopped is fired after a run is stopped
    property OnAfterRunStopped : TNotifyEvent read FOnAfterRunStopped write SetOnAfterRunStopped;

    // OnPrepareInitialGenotype is called with every genotype in the initial
    // population. Here, the application developer has a chance to alter the
    // genotypes before evolution starts.
    property OnPrepareInitialGenotype : TOnPrepareInitialGenotype read FOnPrepareInitialGenotype write SetOnPrepareInitialGenotype;

    // OnGenotypeIteration will be fired each iteration when
    // GenotypeIterationEvent is set to true
    property OnGenotypeIteration : TOnGenotypeIteration read FOnGenotypeIteration write SetOnGenotypeIteration;
  end;

  TNNPopulation = class(TNEATPopulation);

  // TSplitInnovation is used to keep track of the split innovations that occur
  // during a generation
  TSplitInnovation = class
  private
    FNewNodeID: integer;
    FFirstInnovationID: integer;
    FCount: integer;
    FInnovationID: integer;
    FSecondInnovationID: integer;
  public
    // InnovationID is the connection that was split
    property InnovationID : integer read FInnovationID;

    // The NewNodeID is the ID of the new node that was created
    property NewNodeID : integer read FNewNodeID;

    // The two connections that were created in order to handle the new innovation
    property FirstInnovationID  : integer read FFirstInnovationID;
    property SecondInnovationID : integer read FSecondInnovationID;

    // Count keeps track of how many times this innovation was used
    property Count : integer read FCount;
  end;

  TCacheHandler = class
  protected
    // A list of cached nodes
    FNodeCache : TNodeList;

    // A list of cached connects
    FConnectCache : TConnectList;

  public
    // Clear the entire cache, freeing all cached objects
    procedure ClearCache;

    // If there is a cached node, return it, otherwise create a new one
    function GetOrCreateNode(Genotype : TGenotype) : TNode;

    // If there is a cached connect, return it, otherwise create a new one
    function GetOrCreateConnect(Genotype : TGenotype) : TConnect;

    // AddNodeToCache adds a node to the cache, it will also prepare the node
    // for caching
    procedure AddNodeToCache(Node : TNode);

    // AddConnectToCache adds a conenct to the cache, it will also prepare the
    // connect for caching
    procedure AddConnectToCache(Connect : TConnect);

    constructor Create;
    destructor Destroy;override;
  end;

  function Sigmoid(x : double; slope : double = 1.0) : double;
  function AdjustedSigmoid(x : double) : double;
  function AdjustedGaussian(x : double) : double;

  function FloatToBool(f : double) : boolean;
  function BoolToFloat(b : boolean) : double;

  procedure RegisterNewTransferFuntion(Heading, Name : string; TransferFunction : TTransferFunction);

  function NEATCreatedAndDestroyed : string;

  procedure Register;

var
  TransferFunctionList : TStringList;

  CacheHandler : TCacheHandler;

  G_ConnectCreated : integer=0;
  G_ConnectDestroyed : integer=0;

  G_NodeCreated : integer=0;
  G_NodeDestroyed : integer=0;

  G_GenotypeCreated : integer=0;
  G_GenotypeDestroyed : integer=0;

  G_SpeciesCreated : integer=0;
  G_SpeciesDestroyed : integer=0;

implementation

uses
  fFitnessMonitor, uTransferFunctionClasses;

procedure Register;
begin
  //RegisterComponents('NEAT',[TNEATPopulation, TNNPopulation]);
  RegisterComponents('NEAT',[TNEATPopulation]);
end;

function NEATCreatedAndDestroyed : string;
var
  s : String;
begin
  s := Format('Connects = %d, G_ConnectCreated = %d, G_ConnectDestroyed = %d',[G_ConnectCreated-G_ConnectDestroyed, G_ConnectCreated, G_ConnectDestroyed])+#13#10;
  s := s + Format('Nodes = %d, G_NodeCreated = %d, G_NodeDestroyed = %d',[G_NodeCreated-G_NodeDestroyed, G_NodeCreated, G_NodeDestroyed])+#13#10;
  s := s + Format('Genotypes = %d, G_GenotypeCreated = %d, G_GenotypeDestroyed = %d',[G_GenotypeCreated-G_GenotypeDestroyed, G_GenotypeCreated, G_GenotypeDestroyed])+#13#10;
  s := s + Format('Species = %d, G_SpeciesCreated = %d, G_SpeciesDestroyed = %d',[G_SpeciesCreated-G_SpeciesDestroyed, G_SpeciesCreated, G_SpeciesDestroyed])+#13#10;
  s := s + Format('NodeCached = %d, ConnectCache = %d',[CacheHandler.FNodeCache.Count, CacheHandler.FConnectCache.Count]);

  result := s;
end;

function FloatToBool(f : double) : boolean;
begin
  {$ifdef ZERO_IS_MIN}
  result := (f>0.5);
  {$else}
  result := (f>0);
  {$endif}
end;

function BoolToFloat(b : boolean) : double;
begin
  {$ifdef ZERO_IS_MIN}
  result := IfThen(b, 1, 0);
  {$else}
  result := IfThen(b, 1, -1);
  {$endif}
end;

procedure RegisterNewTransferFuntion(Heading, Name : string; TransferFunction : TTransferFunction);
begin
  // Make sure that someone else hasn't created it allready
  if TransferFunctionList=nil then
    TransferFunctionList := TStringList.Create;

  // Make sure that we don't allready have a transfer function with that name
  Assert(TransferFunctionList.IndexOf(Name)=-1,Format('There is allready a transferfunction named %s!',[Name]));

  // Add the transfer function to the list
  TransferFunctionList.AddObject(Name, TransferFunction);

  // Update the transferfunction with it's new name
  TransferFunction.TransferFunctionName := Name;
end;

function Sigmoid(x : double; slope : double = 1.0) : double;
begin
  result := (1/(1+exp(-(slope * x))));
end;

function AdjustedSigmoid(x : double) : double;
const
  cSLOPE = 4.924273;
  cCONSTANT= 2.4621365;

begin
  {$ifdef ZERO_IS_MIN}
  //result := 1/(1+exp(-(cSLOPE*x + cCONSTANT)));
  //return (1/(1+(exp(-(slope*activesum))))); //Compressed
  result := 1/(1+exp(-cSLOPE*x));
  {$else}
  result := (2/(1+exp(-cSLOPE*x)))-1;
  {$endif}
end;

function AdjustedGaussian(x : double) : double;
begin
  {$ifdef ZERO_IS_MIN}
  result := exp(-sqr(x*2.5));
  {$else}
  result := 2*exp(-sqr(x*2.5))-1;
  {$endif}
end;

function Restrict(x : double) : double;
begin
  result := Max(-1, Min(1, x));
end;

{ TNodeList }

function TNodeList.GetItems(i: integer): TNode;
begin
  result := TNode(Get(i));
end;

function TNodeList.GetKeyForItem(Item: Pointer): cardinal;
begin
  result := TNode(Item).FNodeID;
end;

procedure TNodeList.SetItems(i: integer; const Value: TNode);
begin
  Put(i, Value);
end;

{ TConnectList }

function TConnectList.GetItems(i: integer): TConnect;
begin
  result := TConnect(Get(i));
end;

function TConnectList.GetKeyForItem(Item: Pointer): cardinal;
begin
  result := TConnect(Item).FInnovationID;
end;

procedure TConnectList.SetItems(i: integer; const Value: TConnect);
begin
  Put(i, Value);
end;

{ TGenotype }

procedure TGenotype.AddRandomConnection;
var
  Connect : TConnect;
  tries : integer;
  failed : boolean;
  Head, Tail : TNode;
  HeadID, TailID : integer;
begin
  // Reset the tries counter
  tries := 0;

  // Assume failiure
  Failed := true;

  // Assign values for easier debugging
  TailID := -1;
  HeadID := -1;

  // First, try to find two nodes that aren't allready connected!
  while (tries<=ADD_CONNECTION_TRIES) and Failed do
  begin
    // Increase the tries counter
    inc(tries);

    // Get a tail node - don't avoid the input nodes
    Tail := GetRandomNode(false);
    TailID := Tail.NodeID;

    // Get a head node - avoid the input nodes
    Head := GetRandomNode(true);
    HeadID := Head.NodeID;

    // If AllowRecurrentLinks is false, connections from node to self is illegal
    if not NEATPopulation.AllowRecurrentLinks and (Tail = Head) then
    begin
      // We failed
      failed := true;

      // Try the loop again
      continue;
    end;

    // Check all connections, are there any that are identical?
    if ConnectionExists(TailID, HeadID) then
    begin
      // We failed
      failed := true;

      // Try the loop again
      continue;
    end;

    // If cyclic connects aren't allowed, and this is a recurrent connect, then
    // we've failed
    if not NEATPopulation.AllowRecurrentLinks and (Head.YPos<=Tail.YPos) then
    begin
      // We failed
      failed := true;

      // Try the loop again
      continue;
    end;

    // We didn't fail!
    Failed := false;
  end;

  // If we failed, then just get out of here, there will be no new connection
  if Failed then
    exit;//}

  // Create a new connection and hook it up
  Connect := CacheHandler.GetOrCreateConnect(self);

  // Get a tail node - don't avoid the input nodes
  Connect.FTailNodeID := TailID;

  // Get a head node - avoid the input nodes
  Connect.FHeadNodeID := HeadID;

  // Set the initial weight
  Connect.FWeight := NEATPopulation.GetRandomWeight;

  // The new connection must be enabled
  Connect.FEnabled := true;

  // Get the innovation number for the new connection
  Connect.FInnovationID := NEATPopulation.GetNextConnectInnovation(Connect);

  // Set a delay
  Connect.Delay := NEATPopulation.GetRandomConnectionDelay;

  // Add the connection to the list of connections
  ConnectList.Add(Connect);

  // Make sure that it's a valid connection
  Assert(Connect.CheckConnectValidity, Format('%d AddRandomConnection failed.',[Connect.InnovationID]));
end;

procedure TGenotype.CacheNodesAndConnects;
var
  i : integer;
  NodeConnectCount : integer;
begin
  // This will be used to make sure that the ConnectList and the ConnectLists
  // of the nodes agree
  NodeConnectCount := 0;

  // Cache all nodes
  for i := 0 to NodeList.Count-1 do
  begin
    // Increase the connect count
    NodeConnectCount := NodeConnectCount + NodeList[i].EnabledConnectList.Count;

    // Cache the node
    CacheHandler.AddNodeToCache(NodeList[i]);
  end;

  // Clear the nodelist
  NodeList.Clear;

  // Make sure that the ConnectList and the ConnectLists of the nodes agree. The
  // NodeConnectValue might be lower, due to the fact that disabled nodes aren't
  // present in the Node.ConnectList - but they should be present in
  // Genotype.ConnectList
  Assert(NodeConnectCount<=ConnectList.Count,
    Format('There is an error in counting connections, ConnectList.Count = %d <>  NodeConnectCount = %d!',[ConnectList.Count, NodeConnectCount]));

  // Cache all connects
  for i := 0 to ConnectList.Count-1 do
    CacheHandler.AddConnectToCache(ConnectList[i]);

  // Clear the ConnectList
  ConnectList.Clear;
end;

function TGenotype.Copy: TGenotype;
var
  NewGenotype : TGenotype;
begin
  NewGenotype := TGenotype.Create(NEATPopulation);
  CopyTo(NewGenotype);

  result := NewGenotype;
end;

procedure TGenotype.CopyTo(Genotype: TGenotype);
var
  i : integer;
  NewNode : TNode;
  NewConnect : TConnect;
begin
  // Suggest that the new genotype should belong to the same species as this
  // genotype
  Genotype.SpeciesHint := SpeciesHint;

  // Make sure the node is empty
  Assert(Genotype.NodeList.Count=0, 'Still nodes in the Genotype!');
  Assert(Genotype.ConnectList.Count=0, 'Still connects in the Genotype!');

  for i := 0 to NodeList.Count-1 do
  begin
    // This would remove the node
    {if NodeList[i].TouchOfDeath and (random<=DELETE_UNUSED_NODE_CHANCE) then
      continue;//}

    NewNode := NodeList[i].Copy;
    NewNode.Genotype := Genotype;
    Genotype.NodeList.Add(NewNode);
  end;

  for i := 0 to ConnectList.Count-1 do
  begin
    // This would remove the connect
    {if ConnectList[i].TouchOfDeath and (random<=DELETE_UNUSED_CONNECTION_CHANCE) then
      continue;//}

    NewConnect := ConnectList[i].Copy;
    NewConnect.FGenotype := Genotype;
    Genotype.ConnectList.Add(NewConnect);
  end;

  Genotype.Fitness := Fitness;
  Genotype.AdjustedFitness := AdjustedFitness;

  Genotype.FNEATPopulation := NEATPopulation;

  // The new genotype has the same depth as this genotype
  Genotype.MaxNodeDepth := MaxNodeDepth;
end;

constructor TGenotype.Create(ANEATPopulation : TNEATPopulation);
begin
  FNEATPopulation := ANEATPopulation;
  FNodeList := TNodeList.Create(cSortedNodeConnectLists);
  FNodeEvaluationList := TNodeList.Create(false);
  FConnectList := TConnectList.Create(cSortedNodeConnectLists);
  FInputNodes := TNodeList.Create(false);
  FOutputNodes := TNodeList.Create(false);
  FSpeciesHint := nil;
  FPhenotypePrepared := false;

  inc(G_GenotypeCreated);
end;

procedure TGenotype.Cross(Mother, Father: TGenotype);
var
  i : integer;
  ChildNode, FatherNode : TNode;
  ChildConnect, FatherConnect : TConnect;
  AveragingCrossover : boolean;
  TempYPos : double;
begin
  // ** Note, the mother is always more fit, or equal, to the father. This means
  // that copying the mother into the child means that no "excess" genes are
  // copied from the less fit parent.

  // Make sure that the parents were provided
  Assert(Assigned(Mother),'Mother must be set!');
  Assert(Assigned(Father),'Father must be set!');

  // Copy all the nodes and connects of the mother to the child
  Mother.CopyTo(self);

  // For each node in the father
  for i := 0 to Father.NodeList.Count-1 do
  begin
    // Retreive the father node
    FatherNode := Father.NodeList[i];

    // If it's an input node, there's no point in doing anything more
    if FatherNode.IsInputNode then
      continue;

    // Retreive the child node
    ChildNode := GetNodeByID(FatherNode.NodeID);

    // If the node exists in the child, then copy if from the father 50% of the
    // time
    if Assigned(ChildNode) and (random>0.5) then
    begin
      // Save the YPos of the child
      TempYPos := ChildNode.YPos;

      // Copy the fathernode to self
      FatherNode.CopyTo(ChildNode);

      // Set the correct genotype of the childnode
      ChildNode.Genotype := self;

      // Reset the ypos (this can't be changed by crossover)
      ChildNode.YPos := TempYPos;
    end;
      // Otherwise, keep the node that was copied from the mother!
  end;

  // For each connection in the father
  for i := 0 to Father.ConnectList.Count-1 do
  begin
    // Retreive the father node
    FatherConnect := Father.ConnectList[i];

    // Retreive the child connect
    ChildConnect := GetConnectByID(FatherConnect.InnovationID);

    // If it exists in the child, and either of the two connections are disabled,
    // there's a 75% chance of the connection being disabled
    {if Assigned(ChildConnect) and ((not ChildConnect.Enabled) or (not FatherConnect.Enabled)) and
      (random<=0.75) then
    begin
      // Disable the child
      ChildConnect.Enabled := False;

      // Move along
      continue;
    end;//}

    // If the connect exists in the child (and thus, existed in the mother),
    // then copy if from the father 50% of the time.
    if Assigned(ChildConnect) and (random>0.5) then
    begin
      // Should this be an averaging crossover?
      AveragingCrossover := (random<=AVERAGING_CROSSOVER_CHANCE);

      // Is it averaging crossover?
      if AveragingCrossover then
      begin
        // Set the weight to the average of the mother and the father!
        ChildConnect.FWeight := (ChildConnect.Weight + FatherConnect.Weight)/2;
      end else
      begin
        // Copy the fathernode to self
        FatherConnect.CopyTo(ChildConnect);

        // Set the correct genotype of the childnode
        ChildConnect.FGenotype := self;
      end;
    end;

    // If it's disable, enable it in 25 % of the cases
    if Assigned(ChildConnect) and not ChildConnect.Enabled and (random<=0.25) then
      ChildConnect.FEnabled := true;

      // Otherwise, keep the connect that was copied from the mother!
  end;//}
end;

destructor TGenotype.Destroy;
var
  i : integer;
begin
  try
    // This should allready be empty!
    for i := 0 to NodeList.Count-1 do
      NodeList[i].Free;

    // This should allready be empty!
    for i := 0 to ConnectList.Count-1 do
      ConnectList[i].Free;

    NodeList.Free;
    NodeEvaluationList.Free;
    ConnectList.Free;

    InputNodes.Free;
    OutputNodes.Free;
  finally
    inc(G_GenotypeDestroyed);
    inherited;
  end;
end;

{ TTGenotypeList }

function TGenotypeList.GetItems(i: integer): TGenotype;
begin
  result := TGenotype(Get(i));
end;

procedure TGenotypeList.SetItems(i: integer; const Value: TGenotype);
begin
  Put(i, Value);
end;

function TGenotype.GetConnectByID(Id: integer): TConnect;
begin
  result := TConnect(ConnectList.GetItemForKey(ID));
end;

function TGenotype.GetNodeByID(NodeId: integer): TNode;
begin
  result := TNode(NodeList.GetItemForKey(NodeID));
end;

function TGenotype.GetRandomActiveConnect: TConnect;
var
  test : integer;
begin
  result := nil;
  test := 15;
  while not Assigned(result) and (test>0) do
  begin
    result := GetRandomConnect;
    if Assigned(result) and not result.Enabled then
      result := nil;
    dec(test);
  end;
end;

function TGenotype.GetRandomConnect: TConnect;
begin
  if ConnectList.Count=0 then
    result := nil
  else
    result := ConnectList[random(ConnectList.Count)];
end;

function TGenotype.GetRandomNode(ForbidInput : boolean): TNode;
var
  Node : TNode;
begin
  Assert(NodeList.Count>0,'No nodes to choose from!');

  repeat
    Node := NodeList[random(NodeList.Count)];
  until (not ForbidInput) or (not Node.IsInputNode);

  // Make sure that the picked node is valid!
  Assert(Node.CheckNodeValidity, 'GetRandomNode failed');

  result := Node;
end;

procedure TGenotype.Iterate(cnt : integer = -1);
const
  FAIL_COUNT = 5;
var
  Iterations,i : integer;
  Node : TNode;
  IterateUntilStabilized : boolean;
begin
  IterateUntilStabilized := false;

  // If cnt=-1 then we should use the default iteration count
  if cnt=-1 then
  begin
    // If NEATPopulation.ActivationIterations <= 0, that means we should iterate
    // MaxNodeDepth times

    if NEATPopulation.ActivationIterations=0 then
      cnt := MaxNodeDepth - 1
    else if NEATPopulation.ActivationIterations=-1 then
    begin
      // Maximum number of iterations
      cnt := 20;
      IterateUntilStabilized := true;
    end else
      cnt := NEATPopulation.ActivationIterations;
  end;

  // Make sure that cnt is at least 1
  Assert(cnt>0,'At least 1 iteration is required!');

  // Make sure that the phenotype has been prepared!
  Assert(PhenotypePrepared,'The phenotype hasn''t been prepared, can''t iterate!');

  // Initialize all variables
  Iterations := 0;

  // Show this as an event?
  if NEATPopulation.GenotypeIterationEvent and
    Assigned(NEATPopulation.OnGenotypeIteration) then
      NEATPopulation.OnGenotypeIteration(NEATPopulation, self, isPreIterations);

  // Continue until all outputs are on AND the iterations have been completed
  while (Iterations<cnt) do
  begin
    // For each node in the NodeEvaluationList.
    for i := 0 to NodeEvaluationList.Count-1 do
    begin
      // Retrieve the node
      Node := NodeEvaluationList[i];

      // If it's not an input node, calculate the new value!
      if not Node.IsInputNode {and not Node.IsOutputNode//} then
        // Calculate the value
        Node.Value := Node.CalculateValue;
    end;

    // Stabilized yet?
    if IterateUntilStabilized and Stabilized then
    begin
      // Stabilized and done!
      break;
    end;

    // Show this as an event?
    if NEATPopulation.GenotypeIterationEvent and
      Assigned(NEATPopulation.OnGenotypeIteration) then
        NEATPopulation.OnGenotypeIteration(NEATPopulation, self, isMidIterations);

    // This iteration is done, copy all values to OldValue
    for i := 0 to NodeEvaluationList.Count-1 do
    begin
      // Retrieve the node for easy access
      Node := NodeEvaluationList[i];

      // If it's not an inputnode, push the value to old value. (This could
      // be done for inputnodes too, the values are actually the same)
      if not Node.IsInputNode then
        Node.OldValue := Node.Value;
    end;

    // Count up the interations counter by 1
    inc(Iterations);
  end;

  // Show this as an event?
  if NEATPopulation.GenotypeIterationEvent and
    Assigned(NEATPopulation.OnGenotypeIteration) then
      NEATPopulation.OnGenotypeIteration(NEATPopulation, self, isPostIterations);
end;

procedure TGenotype.Mutate;
var
  i : integer;
  Connect : TConnect;
begin
  // Add a new connection?
  if random<=NEATPopulation.ConnectionAddChance then
  begin
    // Add a random connection
    AddRandomConnection;

    // Exclusive operation!
    exit;
  end;

  // Split an existing connection?
  if random<=NEATPopulation.ConnectionSplitChance then
  begin
    // Bias to old nodes when the genome is small. To avoid chaining (?)
    if (GetSize-NodeList.Count<15) then
    begin
      // loop through all genes
      for i := 0 to ConnectList.Count-1 do
      begin
        // Is it enabled?
        if not ConnectList[i].Enabled then
          continue;

        // If the connection has a weight of zero, it's not a connection we're
        // interested in
        if ConnectList[i].Weight = 0 then
          continue;

        // If it's indata is from a bias node, don't split the connection
        if GetNodeByID(ConnectList[i].TailNodeID).BiasNode then
          continue;//}

        // 30% chance of picking the connect
        if (random<=0.30) then
        begin
          // Split it
          ConnectList[i].MutateSplit;

          // Jump out of the loop
          break;
        end;//}
      end;
    end else
    begin
      // Try X times to find enabled connection
      i := 0;
      repeat
        // Pick a random connection
        Connect := GetRandomConnect;

        // If it's enabled, split it!
        if Assigned(Connect) and
           Connect.Enabled and
           (Connect.Weight<>0) and
           not Connect.IsRecurrent and
           not GetNodeByID(Connect.TailNodeID).BiasNode{} then
        begin
          // Split
          Connect.MutateSplit;

          // Get out of the loop, because this connect has been disabled in the
          // split
          break;
        end;

        // Increase the try-counter
        inc(i);
      until (i>=20) or Connect.Enabled;//}
    end;

    // Exclusive operation!
    exit;
  end;

  // Mutate connection weight?
  // Note that this can be done in two ways;
  // 1. Mutate ALL weights in the indiviual, X% of the time
  //   or
  // 2. Mutate X% of the weights in the indiviuals, ALL the time.
  // Method 1 seems to work better (50% better) in cases like obstacle
  // navigator, but original NEAT used method 2.
  for i := 0 to ConnectList.Count-1 do
    if random<=NEATPopulation.ConnectionWeightMutationChance then
      ConnectList[i].MutateWeight;

  // Mutate enabled/disabled?
  if random<=CONNECTION_ENABLED_FLIP_CHANCE then
  begin
    Connect := GetRandomConnect;
    if Assigned(Connect) then
      Connect.FlipEnabled;//}
  end;

  for i := NodeList.Count-1 downto 0 do
  begin
    // Mutate the node function? Most often, Sigmoid is the only function...
    if random<=NODE_FUNCTION_MUTATION_CHANCE then
      NodeList[i].MutateFunction;
  end;
end;

function PriorityCompare(Item1, Item2: Pointer): Integer;
var
  Node1, Node2 : TNode;
begin
  Node1 := TNode(Item1);
  Node2 := TNode(Item2);

  // If they have the same priority, use the nodeid instead.
  if (Node1.Priority = Node2.Priority) then
    result := Node1.NodeID-Node2.NodeID
  else
    // If they have different priority, use the priority number instead, since
    // higher priority means "execute sooner", we sort in reverse order
    result := Node2.Priority-Node1.Priority;//}
end;

procedure TGenotype.PreparePhenotype;
var
  i : integer;
  ToNode : TNode;
  Node : TNode;
begin
  InputNodes.Clear;
  OutputNodes.Clear;

  for i := 0 to NodeList.Count-1 do
  begin
    Node := NodeList[i];

    Node.EnabledConnectList.Clear;

    if Node.IsInputNode then
      InputNodes.Add(Node);

    if Node.IsOutputNode then
      OutputNodes.Add(Node);

    Node.FTouchOfDeath := not (Node.IsInputNode or Node.IsOutputNode);
  end;

  for i := 0 to ConnectList.Count-1 do
    with ConnectList[i] do
    begin
      if Enabled then
      begin
        FTailNode := GetNodeByID(TailNodeID);

        // Broken link?
        if TailNode=nil then
        begin
          FTouchOfDeath := true;
          Continue;
        end;

        ToNode := GetNodeByID(HeadNodeID);

        if (ToNode<>nil) then
        begin
          ToNode.EnabledConnectList.Add(ConnectList[i]);

          TailNode.FTouchOfDeath := false;
          ToNode.FTouchOfDeath := false;
        end else
          FTouchOfDeath := true;
      end;
    end;

  // Copy the nodes to the NodeEvaluationList
  NodeEvaluationList.Assign(NodeList);

  // Sort the NodeEvaluationList
  {$ifdef USE_PRIORITY}
  NodeEvaluationList.Sort(PriorityCompare);//}
  {$endif}

  // Register that the phenotype has now been prepared
  FPhenotypePrepared := true;
end;

function TGenotype.Identical(Genotype: TGenotype): boolean;
var
  i : integer;
  Node1, Node2 : TNode;
  Conn1, Conn2 : TConnect;
begin
  result := false;

  if NodeList.Count<>Genotype.NodeList.Count then
    exit;

  if ConnectList.Count<>Genotype.ConnectList.Count then
    exit;

  for i := 0 to ConnectList.Count-1 do
  begin
    Conn1 := ConnectList[i];
    Conn2 := Genotype.ConnectList[i];

    if (Conn1.InnovationID<>Conn2.InnovationID) or
       (Conn1.Delay<>Conn2.Delay) or
       (Conn1.Enabled<>Conn2.Enabled) or
       (Conn1.Weight<>Conn1.Weight) then
      exit;
  end;

  for i := 0 to NodeList.Count-1 do
  begin
    Node1 := NodeList[i];
    Node2 := Genotype.NodeList[i];

    if (Node1.NodeID<>Node2.NodeID) or
       (Node1.TransferFunction<>Node2.TransferFunction) then
      exit;
  end;

  result := true;
end;

function TGenotype.Unrelatedness(Genotype: TGenotype): double;
var
  i : integer;
  MyConnect, HisConnect : TConnect;
  DeltaW, DeltaDelay : double;

  {$ifdef COUNT_NODES_IN_UNRELATEDNESS}
  MyNode, HisNode : TNode;
  NodeNotCommon : integer;
  {$endif}
  ConnectNotCommon : integer;
  ConnectCommon : integer;
  DifferentFunction : integer;
begin
  // Pick the sum of nodes and connects from the bigger genotype
  // N := Max(GetSize, Genotype.GetSize);

  // Clear the delta weight variable
  DeltaW := 0;
  DeltaDelay := 0;

  // Clear the gene counters
  {$ifdef COUNT_NODES_IN_UNRELATEDNESS}
  NodeNotCommon := 0;
  {$endif}
  ConnectNotCommon := 0;
  DifferentFunction := 0;
  ConnectCommon := 0;

  {$ifdef COUNT_NODES_IN_UNRELATEDNESS}
  // For each node
  for i := 0 to NodeList.Count-1 do
  begin
    // Retrive the local node
    MyNode := NodeList[i];

    // See if the same node exists in the Genotype
    HisNode := Genotype.GetNodeByID(MyNode.NodeID);

    // If both have the node, handle that
    if Assigned(HisNode) then
    begin
      // Keep track of how often the function was different
      if (MyNode.TransferFunction<>HisNode.TransferFunction) then
        inc(DifferentFunction);
    end else
      // Increase the NodesInCommon counter
      inc(NodeNotCommon);
  end;
  {$endif}

  // The number of nodes in self that are also present in Genotype are given by
  // NodeList.Count-NodeNotCommon. There must therefore be an extra
  // Genotype.Nodelist.Count-(NodeList.Count-NodeNotCommon)
  // nodes that are NOT in common
  {$ifdef COUNT_NODES_IN_UNRELATEDNESS}
    NodeNotCommon := NodeNotCommon + Genotype.Nodelist.Count-(NodeList.Count-NodeNotCommon);
  {$else}
  {$endif}


  // For each connect
  for i := 0 to ConnectList.Count-1 do
  begin
    // Retrive the local connect
    MyConnect := ConnectList[i];

    // See if the same connect exists in the Genotype
    HisConnect := Genotype.GetConnectByID(MyConnect.InnovationID);

    // If both have the connect, handle that
    if Assigned(HisConnect) then
    begin
      // Keep track of the difference in weights
      DeltaW := DeltaW + abs(MyConnect.Weight-HisConnect.Weight);

      DeltaDelay := DeltaDelay + abs(MyConnect.Delay-HisConnect.Delay);

      // Another connect in common
      inc(ConnectCommon);
    end else
      // Increase the ConnectionsInCommon counter
      inc(ConnectNotCommon);
  end;

  // Using the exact same argument as above
  ConnectNotCommon := ConnectNotCommon + Genotype.ConnectList.Count - (ConnectList.Count - ConnectNotCommon);

  // Calculate the relatedness
  //result := NodeNotCommon/N + ConnectNotCommon/N + 0.5*DifferentFunction/N + 0.4*DeltaW;

  // DeltaW should be averaged over the number of connections
  if (ConnectCommon<>0) then
    DeltaW := DeltaW / ConnectCommon
  else
    DeltaW := 0;//}

  result :=
    ConnectNotCommon +
    NEATPopulation.WeightUnrelatednessFactor * DeltaW  +
    cDifferentConnectionDelayUnrelatednessWeight * DeltaDelay +
    cDifferentFunctionUnrelatednessWeight * DifferentFunction;

  {$ifdef COUNT_NODES_IN_UNRELATEDNESS}
    result := result +
      NodeNotCommon;
  {$endif}
end;

procedure TGenotype.Flush;
var
  i : integer;
begin
  PrepareInputs;
  
  for i := 0 to NodeList.Count-1 do
    NodeList[i].Flush;

  for i := 0 to ConnectList.Count-1 do
    ConnectList[i].Flush;
end;

function TGenotype.SaveToString: string;
var
  i : integer;
  s : string;
  Connect : TConnect;
begin
  s := '';

  for i := 0 to NodeList.Count-1 do
    with NodeList[i] do
      s := s + SaveToString + #13#10;

  // Space between the two lists
  s := s + #13#10;

  for i := 0 to ConnectList.Count-1 do
  begin
    Connect := ConnectList[i];

    s := s + Format('C %d : %d -> %d (Weight=%f%s, Delay=%d)',
      [Connect.InnovationID,
       Connect.TailNodeID,
       Connect.HeadNodeID,
       Connect.Weight,
       CL_IfThen(Connect.Enabled,'',', DISABLED'),
       Connect.Delay])+#13#10;
  end;

  result := s;
end;

procedure TGenotype.SetupInitialGenotype(Inputs, Outputs: integer);
var
  i,j,gpos,cpos : integer;
  Node : TNode;
  Connect : TConnect;
begin
  gpos := 0;
  cpos := 0;
  for i := 0 to Inputs-1 do
  begin
    Node := CacheHandler.GetOrCreateNode(self);
    Node.NodeID := gpos;
    Node.IsInputNode := true;
    Node.YPos := 0;

    // Inputs should always be sigmoid
    Node.TransferFunction := TTransferFunction(TransferFunctionList.Objects[0]);

    NodeList.Add(Node);
    inc(gpos);
  end;

  for i := 0 to Outputs-1 do
  begin
    Node := CacheHandler.GetOrCreateNode(self);
    Node.NodeID := gpos;
    Node.IsOutputNode := true;
    Node.MutateFunction;
    Node.YPos := 100;

    {$ifdef ONLY_ALLOW_SIGMOID_IN_OUTPUT}
    Node.TransferFunction := TTransferFunction(TransferFunctionList.Objects[0]);
    {$endif}

    NodeList.Add(Node);

    if NEATPopulation.ConnectOutputOfInitialPop then
    begin
      Connect := CacheHandler.GetOrCreateConnect(self);

      // Randomize the weight
      Connect.FWeight := NEATPopulation.GetRandomWeight;

      // 90% chance of connection
      Connect.FEnabled := true;

      if random>=0.9 then
        Connect.FlipEnabled;

      Connect.FTailNodeID := gpos;
      Connect.FHeadNodeID := gpos;
      Connect.FInnovationID := cpos;
      Connect.FDelay := NEATPopulation.GetRandomConnectionDelay;
      inc(cpos);

      ConnectList.Add(Connect);
    end;

    // Create a link from this node to every inputnode
    if NEATPopulation.ConnectNodesOfInitialPop then
      for j := 0 to Inputs-1 do
      begin
        Connect := CacheHandler.GetOrCreateConnect(self);

        // Randomize the weight
        Connect.FWeight := NEATPopulation.GetRandomWeight;

        // 90% chance of connection
        Connect.FEnabled := true;

        if random>=0.9 then
          Connect.FlipEnabled;

        Connect.FTailNodeID := j;
        Connect.FHeadNodeID := gpos;
        Connect.FInnovationID := cpos;
        Connect.FDelay := NEATPopulation.GetRandomConnectionDelay;
        inc(cpos);

        ConnectList.Add(Connect);
      end;
    inc(gpos);
  end;

  // The initial depth of genotypes is 2, input+output
  MaxNodeDepth := 2;
end;

function TGenotype.GetSize: integer;
var
  i, ConnectCount : integer;
begin
  // Make sure that the genotype is ok
  Assert(Assigned(NodeList) and Assigned(ConnectList),'Lists not assigned!');

  // Count the number of enabled connection genes
  ConnectCount := 0;
  for i := 0 to ConnectList.Count-1 do
    if ConnectList[i].Enabled then inc(ConnectCount);

  // Sum up the number of genes
  result := NodeList.Count + ConnectCount;
end;

function TGenotype.OutputsOff: boolean;
var
  i  : integer;
begin
  // Assume there are nonde
  result := false;

  // Check each outputnode
  for i := 0 to OutputNodes.Count-1 do
    if OutputNodes[i].ActivationCount=0 then
    begin
      // There are outputs that are off
      result := true;

      // We're outa here
      exit;
    end;
end;

function TGenotype.AddNewNode(NodeID : integer): integer;
var
  Node : TNode;
begin
  Node := CacheHandler.GetOrCreateNode(self);

  if NodeID=-1 then
    Node.NodeID := NEATPopulation.GetNextNodeInnovation(Node)
  else
    Node.NodeID := NodeID;

  NodeList.Add(Node);

  result := Node.NodeID;
end;

function TGenotype.AddConnection(TailNodeID, HeadNodeID: integer;
  Weight: double) : TConnect;
var
  Connect : TConnect;
begin
  // Make sure that the nodes are present in the Genotype
  Assert(Assigned(GetNodeByID(TailNodeID)), Format('TailNodeID %d can''t be found in genotype',[TailNodeID]));
  Assert(Assigned(GetNodeByID(HeadNodeID)), Format('HeadNodeID %d can''t be found in genotype',[HeadNodeID]));

  Connect := CacheHandler.GetOrCreateConnect(self);

  // Randomize the weight
  Connect.FWeight := Weight;

  Connect.FEnabled := true;
  Connect.FTailNodeID := TailNodeID;
  Connect.FHeadNodeID := HeadNodeID;
  Connect.FInnovationID := NEATPopulation.GetNextConnectInnovation(Connect);
  Connect.FDelay := random(NEATPopulation.MaxConnectionDelay);

  ConnectList.Add(Connect);
  result := Connect;
end;

procedure TGenotype.LoadValues(a: array of double);
var
  i : integer;
begin
  // For each value in the array
  for i := Low(a) to High(a) do
    // Load it into the input nodes
    Inputs[i] := a[i];
end;

procedure TGenotype.RemoveConnection(Connect: TConnect);
begin
  // Remove the connect from the connectlist
  ConnectList.Remove(Connect);

  // Free the connect
  CacheHandler.AddConnectToCache(Connect);
end;

function TGenotype.ConnectionExists(TailNodeID,
  HeadNodeID: integer): boolean;
var
  i : integer;
begin
  // Check all connections
  for i := 0 to ConnectList.Count-1 do
    if (ConnectList[i].TailNodeID = TailNodeID) and
       (ConnectList[i].HeadNodeID = HeadNodeID) then
    begin
      // Yes, such a connection exists
      result := true;

      // Get out of here
      exit;
    end;

  // No such connection exists!
  result := false;
end;

function TGenotype.GetInputs(i: integer): double;
begin
  result := InputNodes[i].Value;
end;

function TGenotype.GetOutputs(i: integer): double;
begin
  result := OutputNodes[i].Value;
end;

procedure TGenotype.SetInputs(i: integer; const Value: double);
begin
  InputNodes[i].Value := Value;
end;

procedure TGenotype.SetOutputs(i: integer; const Value: double);
begin
  OutputNodes[i].Value := Value;
end;

function TGenotype.GetHiddenNodeCount: integer;
var
  i : integer;
  cnt : integer;
begin
  cnt := 0;
  for i := 0 to NodeList.Count-1 do
    if not (NodeList[i].IsInputNode or NodeList[i].IsOutputNode) then
      inc(cnt);
  result := cnt;
end;

function TGenotype.Stabilized: boolean;
var
  i : integer;
begin
  // Assume success
  result := true;

  // Don't check the inputs
  for i := 0 to NodeList.Count-1 do
  begin
    // If Value and OldValue aren't equal, then the network isn't stablized
    if NodeList[i].Value <> NodeList[i].OldValue then
    begin
      // Not stabilized
      result := false;

      // No point in going on
      exit;
    end;
  end;
end;

procedure TGenotype.LoadFromFile(FileName: string);
var
  Ext : string;
  Strings : TStringList;
begin
  // Make sure the file exits
  Assert(FileExists(FileName),Format('The file [%s] doesn''t exist!',[FileName]));

  // Retrieve the extension
  Ext := ExtractFileExt(FileName);

  // If it's KNEAT, use the KNEAT loader
  if SameText(Ext, '.KNEAT') then
  begin
    // Retireve the file
    Strings := TStringList.Create;

    try
      // Load the file
      Strings.LoadFromFile(FileName);

      // Parse the file and create the genotype
      LoadKNEATFromStrings(Strings);
    finally
      // Release the storage
      Strings.Free;
    end;
  end

  // If it's DNEAT, use the DNEAT loader
  else if SameText(Ext, '.DNEAT') then
  begin
    // Retireve the file
    Strings := TStringList.Create;

    try
      // Load the file
      Strings.LoadFromFile(FileName);

      // Parse the file and create the genotype
      LoadDNEATFromStrings(Strings);
    finally
      // Release the storage
      Strings.Free;
    end;
  end
  // It's an unknown extension!
  else Assert(false, Format('Extension [%s] in filename [%s] is unknown!',[Ext, FileName]));
end;

procedure TGenotype.LoadDNEATFromStrings(Strings: TStrings);
begin
  { TODO : Implement this function }
end;

procedure TGenotype.LoadKNEATFromStrings(Strings: TStrings);
var
  i, NodeID : integer;
  s, cmd : string;
  Connect : TConnect;
  Node : TNode;
  InputCount, OutputCount : integer;

  function sGetNextArg : string;
  begin
    result := GetBefore(' ', s+' ');
    s := GetAfter(' ', s);
  end;

  function iGetNextArg : integer;
  begin
    result := StrToInt(sGetNextArg);
  end;

  function fGetNextArg : double;
  var
    OldDSep : char;
  begin
    OldDSep := FormatSettings.DecimalSeparator;
    FormatSettings.DecimalSeparator := '.';

    try
      result := StrToFloat(sGetNextArg);
    finally
      FormatSettings.DecimalSeparator := OldDSep;
    end;
  end;

begin
  // Empty this genotype of all nodes and connects
  CacheNodesAndConnects;

  // Clear some state variables
  InputCount := 0;
  OutputCount := 0;

  // Start loading
  for i := 0 to Strings.Count-1 do
  begin
    // Retireve the current line
    s := Strings[i];

    // Retrieve the command
    cmd := sGetNextArg;

    // Is it a node line?
    if SameText(cmd, 'node') then
    begin
      {node 1 3 1 3
      node 2 1 1 1
      node 3 1 1 1
      node 4 1 0 2
      node 5 2 0 0
      node 24 2 0 0
      node 40 1 0 0//}

      // First arg is the node number
      NodeID := AddNewNode(iGetNextArg);

      // Retrieve the actual node
      Node := GetNodeByID(NodeID);

      // Is it an input node?
      if InputCount < NEATPopulation.InputNodeCount then
      begin
        // This is an input
        Node.IsInputNode := true;

        // Increase the input count
        inc(InputCount);

        // Set the ypos to 0
        Node.YPos := 0;
      end else

      // Is it an output node?
      if OutputCount < NEATPopulation.OutputNodeCount then
      begin
        // This is an output
        Node.IsOutputNode := true;

        // Increase the output count
        inc(OutputCount);

        // Set the ypos to 100
        Node.YPos := 100;
      end else
        // Set the ypos to 50
        Node.YPos := 50;

      // Make sure that this isn't the highest innovation id
      NEATPopulation.FNodeInnovationCounter := max(NEATPopulation.NodeInnovationCounter, NodeID+1);

      // The rest of the node-info is un-interesting!
    end;

    // Is it a connect line?
    if SameText(cmd, 'gene') then
    begin
      {gene 1 1 4 -6.80046 0 1 -6.80046 1
      gene 2 2 4 -2.78258 0 2 -2.78258 0
      gene 3 3 4 2.16085 0 3 2.16085 1
      gene 2 2 5 1.24025 0 4 1.24025 1
      gene 2 5 4 10.6773 0 5 10.6773 1
      gene 3 3 24 1.48037 0 67 1.48037 1
      gene 3 24 4 -5.43767 0 68 -5.43767 1
      gene 1 3 5 3.35827 0 117 3.35827 1
      gene 2 2 40 2.21451 0 127 2.21451 1
      gene 2 40 4 -1.10119 0 128 -1.10119 1
      gene 2 3 40 -0.65725 0 191 -0.65725 1//}

      // The arguments are; trait from to weight ? connectid ? enabled

      // Ignore trait
      sGetNextArg;

      // Create a new connection
      Connect := CacheHandler.GetOrCreateConnect(self);

      // Connect the tail
      Connect.FTailNodeID := iGetNextArg;

      // Connect the head
      Connect.FHeadNodeID := iGetNextArg;

      // Set the weight
      Connect.FWeight := fGetNextArg;

      // Ignore ?
      sGetNextArg;

      // Set ConnectID
      Connect.FInnovationID := iGetNextArg;

      // Ignore repetition of weight
      sGetNextArg;

      // Is it enabled or not
      Connect.FEnabled := (iGetNextArg=1);

      // Make sure that this isn't the highest innovation id
      NEATPopulation.FConnectInnovationCounter := max(NEATPopulation.ConnectInnovationCounter, Connect.InnovationID+1);

      // Add the conenction to the connectlist
      ConnectList.Add(Connect);
    end;
  end;

  // We should re-calculate the total depth here, but we're assuming 3 for now
  MaxNodeDepth := 3;

  // Prepare the phenotype so the network can be used
  PreparePhenotype;
end;

function TGenotype.AsCode: string;
var
  i, j : integer;
  Code : string;
  Node : TNode;
  OldDS : char;
  sInterface : string;
  procedure AddLine(s : string);
  begin
    Code := Code + s + #13#10;
  end;
begin
  OldDS := FormatSettings.DecimalSeparator;

  FormatSettings.DecimalSeparator := '.';

  try
    for i := 1 to InputNodes.Count do
      sInterface := sInterface + Format('const Input%d : double; ',[i]);

    for i := 1 to OutputNodes.Count do
      sInterface := sInterface + Format('var Output%d : double; ',[i]);

    // Delete the trailing ";" if there is one
    if RightStr(sInterface, 2)='; ' then
      sInterface := LeftStr(sInterface, length(sInterface)-2);

    // Function header
    AddLine(Format('procedure GenotypeAsCode(%s);',[sInterface]));
    AddLine('var');
    AddLine('  i : integer;');

    // Generate all the variables we'll need
    for i := 0 to NodeList.Count-1 do
      if NodeList[i].IsInputNode then
        AddLine(Format('  Node%dValue, Node%dOldValue : double;',[NodeList[i].NodeID, NodeList[i].NodeID]))
      else
        AddLine(Format('  Node%dValue, Node%dOldValue, Activate%dSum : double;',[NodeList[i].NodeID, NodeList[i].NodeID, NodeList[i].NodeID]));

    AddLine('begin');

    //
    AddLine('  // Set inputs');
    for i := 1 to InputNodes.Count do
      AddLine(Format('  Node%dValue := Input%d; Node%dOldValue := Input%d;',[i,i,i,i]));

    for i := InputNodes.Count to NodeList.Count-1 do
    begin
      // Retireve the node
      Node := NodeList[i];

      // Set it's values to zero
      AddLine(Format('  Node%dValue := 0; Node%dOldValue := 0;',[Node.NodeID,Node.NodeID]));
    end;


    AddLine('');

    AddLine('  // Activate the network');

    AddLine(Format('  for i := 0 to %d do',[MaxNodeDepth-1]));
    AddLine('  begin');


    // Calculate all values from old values
    for i := 0 to NodeList.Count-1 do
      if not NodeList[i].IsInputNode then
      begin
        // Retireve the node
        Node := NodeList[i];

        // Calculate the activation sum
        if Node.EnabledConnectList.Count>0 then
        begin
          AddLine(Format('    Activate%dSum := ',[Node.NodeID]));

          for j := 0 to Node.EnabledConnectList.Count-1 do
            if j <> Node.EnabledConnectList.Count-1 then
              AddLine(Format('      Node%dOldValue * %3.4f + ',[NodeList[i].EnabledConnectList[j].TailNodeID, NodeList[i].EnabledConnectList[j].Weight]))
            else
              AddLine(Format('      Node%dOldValue * %3.4f;',[NodeList[i].EnabledConnectList[j].TailNodeID, NodeList[i].EnabledConnectList[j].Weight]));
        end;


        AddLine(Format('    Node%dValue := fSigmoid(Activate%dSum);',[NodeList[i].NodeID, NodeList[i].NodeID]));
        AddLine('');
      end;

    // Update old values
    for i := 0 to NodeList.Count-1 do
      if not NodeList[i].IsInputNode then
      begin
        // Retireve the node
        Node := NodeList[i];
        AddLine(Format('    Node%dOldValue := Node%dValue;',[Node.NodeID, Node.NodeID]));
      end;

    AddLine('  end;');
    AddLine('');
    AddLine('  // Set outputs');
    for i := 1 to OutputNodes.Count do
      AddLine(Format('  Output%d := Node%dValue;',[i, OutputNodes[i-1].NodeID]));
    AddLine('end;');

    // Return the code
    result := Code;
  finally
    FormatSettings.DecimalSeparator := OldDS;
  end;
end;

procedure TGenotype.AddRandomConnections(const ACount: integer);
var
  i : integer;
begin
  for i := 0 to ACount-1 do
    AddRandomConnection;
end;

procedure TGenotype.DisableRandomConnections(const ACount: integer);
var
  i : integer;
  Connection : TConnect;
begin
  for i := 0 to ACount-1 do
  begin
    Connection := GetRandomActiveConnect;
    if Assigned(Connection) then
      Connection.Enabled := false;
  end;

  PreparePhenotype;
end;

procedure TGenotype.DeleteRandomConnections(const ACount: integer);
var
  i : integer;
  Connection : TConnect;
begin
  for i := 0 to ACount-1 do
  begin
    Connection := GetRandomConnect;
    if Assigned(Connection) then
      RemoveConnection(Connection);
  end;

  PreparePhenotype;
end;

function TGenotype.SetNextInputValue(const AValue: double): integer;
begin
  InputNodes[FNextInputNode].Value := AValue;
  inc(FNextInputNode);
end;

procedure TGenotype.PrepareInputs;
begin
  FNextInputNode := 0;
end;

{ TNEATPopulation }

procedure TNEATPopulation.CalculateFitnesses;
var
  i : integer;
  Genotype : TGenotype;
begin
  // Make sure that OnCalculateFitness is set
  Assert(Assigned(OnCalculateFitness),'You must assign a OnCalculateFitness event!');

  // Clear the BestGenotype variable for use
  FBestGenotype := nil;

  // Reset the fitness sums
  FFitnessSum := 0;
  FAdjustedFitnessSum := 0;

  // For each genotype in the current generation
  for i := 0 to Genotypes.Count-1 do
  begin
    // Retrieve the genotype
    Genotype := Genotypes[i];

    // Calculate the fitness
    FCurrentGenotypeID := i;
    Genotype.Fitness := OnCalculateFitness(self, Genotype);

    // Make sure it's a valid fitness
    Assert(Genotype.Fitness>=0,
      Format('Fitness MUST be zero or above, %f is not a valid fitness.!',[Genotype.Fitness]));

    // Store this fitness as the AdjustedFitness fitness, but it might be
    // overwritten later
    Genotype.AdjustedFitness := Genotype.Fitness;

    // Add upp the fitnesssums
    FFitnessSum := FitnessSum + Genotype.Fitness;
    FAdjustedFitnessSum := AdjustedFitnessSum + Genotype.Fitness;

    // Is the genotype better than the best so far? We don't use the adjusted
    // fitness for this, since that may select a very bad individual. If fitness
    // is equal, pick the smaller one.
    if (BestGenotype=nil) or (Genotype.Fitness>BestGenotype.Fitness) or
      ((Genotype.Fitness=BestGenotype.Fitness) and (Genotype.GetSize<BestGenotype.GetSize)) then
      FBestGenotype := Genotype;

    // Give the computer some breathing room - this could be disabled for
    // slightly better performance but severely impaired user interactivity
    Application.ProcessMessages;

    // If the user terminates the application, then it's time to exit this loop
    if Application.Terminated then
      Abort;
  end;

  Assert(FFitnessSum>0, 'At least one genotype must have a fitness > 0!');

  // Place the genomes in their respective species
  if SelectionType=stSpecies then
    PlaceInSpecies;

  // Fire the OnShowBestIndividual event
  ShowBestIndividual;

  // If we have a fitness monitor, update it! This must be done AFTER
  // ShowBestIndividual, because the user might have changed the fitness
  if Assigned(FitnessMonitor) then
    FitnessMonitor.UpdatePopulationView;
end;

procedure TNEATPopulation.ClearPopulation;
var
  i : integer;
begin
  // This generation is up, and the FGenerationInnovationList isn't valid
  // anymore
  if CONNECTION_INNOV_MEM=-1 then
  begin
    // Delete them all
    DeleteAllConnectInnovations;
  end else
  begin
    // Remember connect innovations!
    while FGenerationInnovationList.Count>CONNECTION_INNOV_MEM do
    begin
      // Delete the first one
      CacheHandler.AddConnectToCache(FGenerationInnovationList[0]);

      // Remove it from the list
      FGenerationInnovationList.Delete(0);
    end;
  end;

  // Same goes for the Split innovation list
  ClearSplitInnovations;

  // Make sure all individuals are still present
  Assert((Genotypes.Count=0) or (Genotypes.Count = PopulationSize), 'Genotypes have dissappeared!');

  // Delete all the old genotypes
  for i := 0 to Genotypes.Count-1 do
  begin
    // Cache what can be cached
    Genotypes[i].CacheNodesAndConnects;

    // Now delete it
    Genotypes[i].Free;
  end;

  // Clear the list of them
  Genotypes.Clear;
end;

procedure TNEATPopulation.ClearSpeciesList(FreeAll : boolean);
var
  i : integer;
  Species : TSpecies;
begin
  // Is it an order to delete all species and start over?
  if FreeAll then
  begin
    // Delete every species
    for i := 0 to SpeciesList.Count - 1 do
      SpeciesList[i].Free;

    // Clear the specieslist
    SpeciesList.Clear;

    // We're out of here!
    exit;
  end;

  // Empty out the species, so that they can be filled again!
  for i := 0 to SpeciesList.Count - 1 do
  begin
    // Retrieve the species
    Species := SpeciesList[i];

    // Free the old defining member
    if Assigned(Species.DefiningMember) then
    begin
      // Cache the parts of the defining member
      Species.DefiningMember.CacheNodesAndConnects;

      // Free the genotype
      FreeAndNil(Species.FDefiningMember);
    end;

    // Copy the champion if there is one
    if Species.Champion<>nil then
      Species.FDefiningMember := Species.Champion.Copy
    else
      // Copy the first member in the list - if there is one!
    if Species.Genotypes.Count>0 then
      Species.FDefiningMember := Species.Genotypes[0].Copy;

    // Clear the old champion
    Species.FChampion := nil;

    // Clear out the list of references to old genotypes. The species don't own
    // them, so we must not delete them!
    Species.Genotypes.Clear;
  end;
end;

constructor TNEATPopulation.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FStopped := true;
  FGenotypes := TGenotypeList.Create;
  FOldGeneration := TGenotypeList.Create;
  FGenerationInnovationList := TConnectList.Create(false);
  FGenerationInnovationList.Duplicates := dupAccept;
  FSplitInnovationList := TList.Create;
  FSpeciesList := TSpeciesList.Create;
  FMinSpeciesRelatedness := 3;
  FReproduceMutateRate := REPRODUCE_MUTATE_RATE;
  FSelectionType := stSpecies;
  FUseFitnessMonitor := true;
  FTargetFitness := 0.1;
  FGenerationsToRun := 65;

  FAllowRecurrentLinks := true;
  FConnectNodesOfInitialPop := true;
  FConnectOutputOfInitialPop := false;
  FActivationIterations := 1;
  FSpeciesFitnessMethod := sfmAverageFitness;
  FInitialWeightMagnitude := 1;

  ConnectionSplitChance := 0.03;
  FConnectionAddChance := 0.05;
  FWeightPeturbationFactor := 2.5;
  FMaxLinkWeight := 3;

  PopulationSize := 150;

  TournamentSize := 7;
  TournamentRange := TournamentSize * 2;

  ConnectionWeightMutationChance := 0.8;
  ConnectionMutationBigChangeChance := 0.1;
  FWeightUnrelatednessFactor := 0.4;

  FMutateCrossed := true;

  FSurvivalThreshold := 0.4;

  FInputNodeCount  := 1;
  FOutputNodeCount := 1;

  FGenotypeIterationEvent := false;
  FMaxConnectionDelay := 0;
end;

procedure TNEATPopulation.CreateNewGeneration;
var
  i,j : integer;
  Parent1, Parent2 : TGenotype;
  NewGenotype : TGenotype;
  Species : TSpecies;
begin
  // Swap the populations, so the old population becomes current, and the
  // current population becomes "old"
  SwapPopulations;

  // Delete all the genotypes from the new population
  ClearPopulation;

  // Is is a species population?
  if SelectionType = stSpecies then
  begin
    // Create the new population
    CreateNewSpeciesBasedGeneration;

    // We're done
    exit;
  end;

  // NOT SPECIES!

  // For each individual
  for i := 0 to PopulationSize-1 do
  begin
    // Retrieve the species of the individual we're about to overwrite- if there
    // is one!
    Species := GetSpeciesForGenotype(OldGeneration[i]);

    // If we're in the spot of the best individual from the current pop, just
    // copy that individual
    if OldGeneration[i]=BestGenotype then
    begin
      // Copy the best individual from the previous generation
      NewGenotype := BestGenotype.Copy;
    end else
    // 25 of all species champions are reproduced!
    if (Species<>nil) and (Species.Champion=OldGeneration[i]) and (random<=0.5) then
    begin
      // Copy the champion from the previous generation
      NewGenotype := OldGeneration[i].Copy;
    end else
    if random<=ReproduceMutateRate then
    begin
       // Create a new child
      NewGenotype := PickAncestor(i, nil).Copy;

      // Mutate the new genotype!
      NewGenotype.Mutate;
    end else
    begin
      // Pick first parent
      Parent1 := PickAncestor(i, nil);

      // Pick second parent
      Parent2 := PickAncestor(i, Parent1);

      // Create a new child
      NewGenotype := TGenotype.Create(self);

      // Mate the parents, making the more fit parent the mother
      if Parent1.Fitness > Parent2.Fitness then
        NewGenotype.Cross(Parent1, Parent2)
      else
        NewGenotype.Cross(Parent2, Parent1);

      // Mutate the new genotype!
      // If the parents are identical, we must mutate
      if (Parent1=Parent2) then
        NewGenotype.Mutate
      else if (random>=MATE_ONLY_PROBABILITY) then
      begin
        if MutateCrossed then
          NewGenotype.Mutate
        else
          // If child is identical to parent1, we mutate it!
          if NewGenotype.Identical(Parent1) then
            NewGenotype.Mutate;
      end
      else
      // If the parents have identical fitness, we might as well mutate - they're
      // probably identical in behaviour
      if (Parent1.Fitness=Parent2.Fitness) and (random<=ConnectionWeightMutationChance) then
        for j := 0 to NewGenotype.ConnectList.Count-1 do
          NewGenotype.ConnectList[j].MutateWeight;//}
    end;

    // Prepare the phenotype of the new genome, so that it can be evaluated
    NewGenotype.PreparePhenotype;

    // Add the new genotype to the list of genotypes
    Genotypes.Add(NewGenotype);
  end;
end;

procedure TNEATPopulation.CreateNewSpeciesBasedGeneration;
var
  i,j : integer;
  Parent1, Parent2 : TGenotype;
  NewGenotype : TGenotype;
  Species : TSpecies;

  function GetAncestor : TGenotype;
  begin
    {$ifdef USE_FIT_PROP_IN_SPECIES}
    result := Species.FitnessPropSelection;
    {$else}
    result := Species.Genotypes[random(Species.Genotypes.Count)];
    {$endif}
  end;
begin
  for i := 0 to SpeciesList.Count-1 do
  begin
    // Retrieve the species for easy access
    Species := SpeciesList[i];

    // If the species is dead don't copy the champ
    if Species.DeadSpecies then
      continue;

    // If the species isn't expecting any offspring, it's as good as dead
    if Species.ExpectedOffspringCount=0 then
      continue;

    // If the species has more than 5 members, _OR_ the champ is the total
    // champ, then copy the champ
    if ((Species.MemberCount>4) or (Species.Champion = BestGenotype)) and
      (Species.ExpectedOffspringCount>0) then
    begin
      // One less offspring to expect
      dec(Species.FExpectedOffspringCount);

      // Copy the champion from the previous generation
      NewGenotype := Species.Champion.Copy;

      // Prepare the phenotype of the new genome, so that it can be evaluated
      NewGenotype.PreparePhenotype;

      // Add the new genotype to the list of genotypes
      Genotypes.Add(NewGenotype);
    end;

    // If the species is expecting some offspring, take care of that!
    while Species.ExpectedOffspringCount>0 do
    begin
      // One less offspring to expect
      dec(Species.FExpectedOffspringCount);

      if random<=ReproduceMutateRate then
      begin
         // Create a new child
        NewGenotype := GetAncestor.Copy;

        // Mutate the new genotype!
        NewGenotype.Mutate;
      end else
      begin
        // Pick first parent
        Parent1 := GetAncestor;

        // Pick second parent
        if random<=MIGRATION_RATE then
          Parent2 := PickAncestorTournament(OldGeneration, 14, false)
        else
          Parent2 := GetAncestor;

        // Create a new child
        NewGenotype := TGenotype.Create(self);

        // Mate the parents, making the more fit parent the mother
        if Parent1.Fitness > Parent2.Fitness then
          NewGenotype.Cross(Parent1, Parent2)
        else
          NewGenotype.Cross(Parent2, Parent1);

        // Mutate the new genotype!
        // If the parents are identical, we must mutate
        if (Parent1=Parent2) then
          NewGenotype.Mutate
        else if (MATE_ONLY_PROBABILITY<=random) then
        begin
          if MutateCrossed then
            NewGenotype.Mutate
          else if NewGenotype.Identical(Parent1) then
          // If child is identical to parent1, we mutate it!
            NewGenotype.Mutate;
        end
        else
        // If the parents have identical fitness, we might as well mutate - they're
        // probably identical in behaviour
        if (Parent1.Fitness=Parent2.Fitness) and (random<=ConnectionWeightMutationChance) then
          for j := 0 to NewGenotype.ConnectList.Count-1 do
            NewGenotype.ConnectList[j].MutateWeight;//}
      end;

      // Prepare the phenotype of the new genome, so that it can be evaluated
      NewGenotype.PreparePhenotype;

      // Add the new genotype to the list of genotypes
      Genotypes.Add(NewGenotype);
    end;
  end;

  // If there are any more genotypes needed...
  while Genotypes.Count < PopulationSize do
  begin
    // Create a new child
    NewGenotype := PickAncestorFitProp(Genotypes, AdjustedFitnessSum).Copy;

    // Mutate the new genotype!
    NewGenotype.Mutate;

    // Prepare the phenotype of the new genome, so that it can be evaluated
    NewGenotype.PreparePhenotype;

    // Add the new genotype to the list of genotypes
    Genotypes.Add(NewGenotype);
  end;
end;

procedure TNEATPopulation.CreatePopulation;
var
  i : integer;
  Genotype : TGenotype;
begin
  // Clear the current population, if there is one
  ClearPopulation;

  // Clear and delete all species
  ClearSpeciesList(true);

  // Fore each genome
  for i := 0 to PopulationSize-1 do
  begin
    // Create a new genotype
    Genotype := TGenotype.Create(self);

    // Assign this population as NEATPopulation
    Genotype.FNEATPopulation := self;

    // Create an initial nn
    Genotype.SetupInitialGenotype(InputNodeCount, OutputNodeCount);

    // If this is the first node restart the innovation counters
    if i = 0 then
    begin
      // Restart the NodeInnovationCounter
      FNodeInnovationCounter := Genotype.NodeList.Count;

      // Restart the ConnectInnovationCounter
      FConnectInnovationCounter := Genotype.ConnectList.Count;
    end;

    // If the user has requested, let the user alter the genotype
    if Assigned(FOnPrepareInitialGenotype) then
      OnPrepareInitialGenotype(self, Genotype);

    // Add the genotype to the list of genotypes
    Genotypes.Add(Genotype);

    // Prepare the phenotype of the new genome, so that it can be evaluated
    Genotype.PreparePhenotype;
  end;
end;

destructor TNEATPopulation.Destroy;
var
  i : integer;
begin
  // Stop the run!
  if not FStopped then
    StopRun;
  
  // Clear the current population
  ClearPopulation;

  // Swap, so the old generation becomes current
  SwapPopulations;

  // Clear what used to be the old population
  ClearPopulation;

  // Free the list of genotypes
  Genotypes.Free;

  // Free the list for old generations
  OldGeneration.Free;

  // Free the list that keeps track of new innovations
  DeleteAllConnectInnovations;
  FGenerationInnovationList.Free;

  // Empty the specieslist
  for i := 0 to SpeciesList.Count - 1 do
    SpeciesList[i].Free;

      // Free the list
  SpeciesList.Free;

  // Clear all split innovations
  ClearSplitInnovations;

  // Free the split innovation list
  FSplitInnovationList.Free;

  inherited;
end;

function TNEATPopulation.GetNextConnectInnovation(Connect : TConnect): integer;
var
  i, cim: integer;
  CTest : TConnect;
  ConnectCopy : TConnect;
begin
  // Check every new innovation. If connect is nil, then we're demanding a new
  // innovation
  if Connect<>nil then
    for i := 0 to FGenerationInnovationList.Count-1 do
    begin
      // Retrieve the connect
      CTest := FGenerationInnovationList[i];

      // Make sure that the connection is actually valid!
      // CTest can't be verified, Genotype doesn't exist anymore!
      //Assert(CTest.CheckConnectValidity,'CTest is broken!');

      // Are they identical?
      if Connect.IdenticalTo(CTest) then
      begin
        // Use the old innovation number
        result := CTest.InnovationID;

        // Move the ctest to the end, if there is a memory!
        if CONNECTION_INNOV_MEM>0 then
        begin
          FGenerationInnovationList.Delete(i);
          FGenerationInnovationList.Add(CTest);
        end;//}

        // We're out of here
        exit;
      end;
    end;

  // No indentical connections existed, return the current innovation number
  result := ConnectInnovationCounter;

  // Create a new innovation number
  inc(FConnectInnovationCounter);

  // Add the new innovation to the FGenerationInnovationList, so we can avoid
  // identical innovations in the future
  cim := CONNECTION_INNOV_MEM;
  if Connect <> nil then
  begin
    if cim<0 then
    begin
      // Just add the innovation
      FGenerationInnovationList.Add(Connect);
    end else
    begin
      // Create the new connect copy
      ConnectCopy := Connect.Copy;

      // Set the new innovation number!
      ConnectCopy.FInnovationID := result;

      // Make sure there isn't allready one in the list!
      for i := 0 to FGenerationInnovationList.Count - 1 do
        if FGenerationInnovationList[i].InnovationID = ConnectCopy.InnovationID then
          Assert(false,'An innovation with that number allready exists!');

      // Add the innovation
      FGenerationInnovationList.Add(ConnectCopy);
    end;
  end;
end;

function TNEATPopulation.GetNextNodeInnovation(Node : TNode): integer;
begin
  result := NodeInnovationCounter;
  inc(FNodeInnovationCounter);
end;

function TNEATPopulation.GetRandomTransferFunction: TTransferFunction;
begin

  // Make sure that there are actually at least one transfer function to choose
  // from
  Assert(Assigned(TransferFunctionList),'The TransferFunctionList hasn''t been created!');
  Assert(TransferFunctionList.Count>0,'There are no registered transfer functions!');

  // Pick a transferfunction!
  result := TTransferFunction(TransferFunctionList.Objects[random(TransferFunctionList.Count)]);
end;

function TNEATPopulation.GetSpeciesForGenotype(
  Genotype: TGenotype): TSpecies;
var
  i : integer;
  Species : TSpecies;
begin
  // Assume failiure!
  result := nil;

  {$ifdef REMEMBER_SPECIES}
  // If there is a species hint, see if that pans out
  if Assigned(Genotype.SpeciesHint) and
     (Genotype.UnRelatedness(Genotype.SpeciesHint.DefiningMember)<MinSpeciesRelatedness) then
  begin
    // Pick the hinted species!
    result := Genotype.SpeciesHint;

    // We're out of here!
    exit;
  end;//}
  {$endif REMEMBER_SPECIES}

  // For each species
  for i := 0 to SpeciesList.Count-1 do
  begin
    // Retrieve the species we're considering
    Species := SpeciesList[i];

    // If the species is dead, just continue
    if Species.DeadSpecies then
      Continue;

    // Is it related enough with the first individual in the species
    if Genotype.UnRelatedness(Species.DefiningMember)<MinSpeciesRelatedness then
    begin
      // Set the species hint of this genotype for the next generation!
      Genotype.SpeciesHint := Species;

      // Return the species!
      result := Species;

      // We're out of here!
      exit;
    end;
  end;
end;

function TNEATPopulation.PickAncestor(i: integer;
  AvoidGenotype: TGenotype): TGenotype;
begin
  result := nil;
  case SelectionType of
    stLocalTournament   : result := PickAncestorLinear(i, AvoidGenotype);
    stSpecies           : result := PickAncestorFromSpecies(AvoidGenotype);
    stTournament        : result := PickAncestorTournament(OldGeneration, TournamentSize, false, AvoidGenotype);
    stFitProp           : result := PickAncestorFitProp(OldGeneration, AdjustedFitnessSum);
  end;
end;

function TNEATPopulation.PickAncestorTournament(GenotypeList: TGenotypeList;
  TournamentSize: integer; UseSpeciesFitness : boolean; AvoidGenotype : TGenotype): TGenotype;
var
  tries : integer;
  Genotype : TGenotype;
  BestGenotype : TGenotype;
begin
  // Make sure that there are a few genotypes in the list!
  Assert(GenotypeList.Count>0,'There are no genotypes in the list!');

  // Clear the BestGenotype
  BestGenotype := nil;

  // For each tournament atempt
  for tries := 0 to TournamentSize-1 do
  begin
    // Retrieve the genotype we're testing
    Genotype := GenotypeList[random(GenotypeList.Count)];

    if (Genotype<>AvoidGenotype) and ((BestGenotype=nil) or (Genotype.AdjustedFitness<BestGenotype.AdjustedFitness)) then
      BestGenotype := Genotype;
  end;

  // Make sure we actually found a genotype!
  if BestGenotype=nil then
    result := GenotypeList[0]
  else
    result := BestGenotype;
end;

function TNEATPopulation.PickAncestorFromSpecies(AvoidGenotype: TGenotype): TGenotype;
var
  Species : TSpecies;
begin
  // If AvoidGenotype is null, then that means this is the FIRST ancestor we're
  // picking.
  if AvoidGenotype = nil then
  begin
    //result := PickAncestorFitProp;
    //result := PickAncestorTournament(OldGeneration, 7, true);

    // Use tournament selection, but use the SpeciesFitness instead of the
    // regular fitness!
    //result := PickAncestorTournament(OldGeneration, 20, true, nil);
    //Species := TournamentSelectSpecies(7);
    //result := PickAncestorTournament(Species.Genotypes, TOURNAMENT_SIZE_2, false, nil);

    // First, pick a species ackording to it's fitness
    Species := PickSpeciesFitnessProp;

    // Now, pick an individual ackording to it's fitness
    //result := PickAncestorFitProp(Species.Genotypes, Species.NormalizedFitnessSum);
    result := Species.FitnessPropSelection;
  end else
  begin
    // Pick a genotype from the same species, we'll use tournament selection
    // with a tournament size of 3!

    // In 1% of the cases, use migration!
    if random<=MIGRATION_RATE then
    begin
      // Pick a gentype at random using tournament selection from the full
      // genotype list. Use species fitness!
      result := PickAncestorTournament(OldGeneration, 7, true, AvoidGenotype);

      // We're out of here
      exit;
    end;

    // Get the correct species
    if AvoidGenotype.SpeciesHint<> nil then
      Species := AvoidGenotype.SpeciesHint
    else//}
      Species := GetSpeciesForGenotype(AvoidGenotype);

    // Make sure that there was an actualy species!
    Assert(Assigned(Species),'No species was found!');

    // Pick a genotype from the species
    result := Species.FitnessPropSelection;
    //result := Species.TournamentSelection(5);
  end;
end;

function TNEATPopulation.PickAncestorLinear(i: integer;
  AvoidGenotype: TGenotype): TGenotype;
var
  Test, BestSoFar : TGenotype;
  pos : integer;
  cnt : integer;
begin
  // Pick the old genotype from this spot!
{  if AvoidGenotype=nil then
  begin
    result := OldGeneration[i];
    exit;
  end;//}

  // Pick a good one within tournament range
  BestSoFar := nil;
  for cnt := 0 to TournamentSize-1 do
  begin
    pos := ((random(TournamentRange)-TournamentRange div 2)+i);
    if pos >= PopulationSize then
      pos := pos - PopulationSize;

    if pos < 0 then
      pos := pos + PopulationSize;

    Test := OldGeneration[pos];

    if (Test<>AvoidGenotype) and ((BestSoFar=nil) or (Test.Fitness>BestSoFar.Fitness)) then
      BestSoFar := Test;
  end;//}

  if BestSoFar=nil then
    BestSoFar := OldGeneration[i];

  result := BestSoFar;
end;

procedure TNEATPopulation.PlaceInSpecies;
var
  i,j,sp : integer;
  Genotype : TGenotype;
  Species : TSpecies;
  SecondBestSpecies : TSpecies;
  SpeciesStagnant : boolean;
  SpeciesOld : boolean;
  MajorStagnation : boolean;

  procedure Speciate;
  var
    i : integer;
  begin
    // Check every individual in the CURRENT population, and place them in the
    // first species where they fit
    for i := 0 to Genotypes.Count-1 do
    begin
      // Retrieve the genotype we're trying to "speciate"
      Genotype := Genotypes[i];

      // Get the correct species to place the genotype in
      Species := GetSpeciesForGenotype(Genotype);

      // If no species was found, create a new one
      if Species=nil then
      begin
        // Create the new species
        Species := TSpecies.Create(self);

        // Set the BirthGeneration
        Species.FBirthGeneration := CurrentGeneration;

        // The species was "improved" this generation
        Species.FLastImprovementGeneration := CurrentGeneration;

        // Set SpeciesID
        Species.FSpeciesID := FSpeciesIDCounter;
        inc(FSpeciesIDCounter);

        // Add the genotypelist as a new species
        SpeciesList.Add(Species);

        // Set the species fitness sums
        Species.FFitnessSum := 0;
        Species.FAdjustedFitnessSum := 0;

        // Set the first member as the defining member
        Species.FDefiningMember := Genotype.Copy;
      end;

      // Set the species hint for the next generation
      Genotype.SpeciesHint := Species;

      // Place the genotype in the species
      Species.Add(Genotype);
    end;
  end;
begin
  // Clear out all the old species
  {$ifdef REMEMBER_SPECIES}
  ClearSpeciesList(false);
  {$else}
  ClearSpeciesList(true);
  {$endif}

  // Reset AdjustedFitnessSum s
  FAdjustedFitnessSum := 0;
  FChampionFitnessSum := 0;
  FChampionAdjustedFitnessSum := 0;

  // Restrict the number of species, if the user has so requested!
  AdjustMinSpeciesRelatedness;

  // Start counting live species
  FLivingSpeciesCount := 0;

  // Prepare SecondBestSpecies
  SecondBestSpecies := nil;

  // Do the speciation
  Speciate;

  // Update the species champions
  for i := SpeciesList.Count-1 downto 0 do
  begin
    // If there is a champ, see if the species has improved
    if Species.Champion <> nil then
    begin
      // Has the species improved?
      if Species.Champion.Fitness>Species.OldChampionFitness then
        Species.FLastImprovementGeneration := CurrentGeneration;

      // Store the current champs fitness
      Species.FOldChampionFitness := Species.Champion.Fitness;

      // Keep track of the second best species
      if (Species<>BestGenotype.SpeciesHint) then
      begin
        if (SecondBestSpecies=nil) or (Species.Champion.Fitness>SecondBestSpecies.Champion.Fitness) then
          SecondBestSpecies := Species;
      end;
    end;
  end;

  // MajorStagnation is when the best individual hasn't improved for more than
  // 20 generations. When this happens, all species but the very best are
  // killed off.
  MajorStagnation := (CurrentGeneration-BestGenotype.SpeciesHint.LastImprovementGeneration)>=MAJOR_STAGNATION_PERIOD;

  // If we have major stagnation, prevent it from happening next time
  if MajorStagnation then
    BestGenotype.SpeciesHint.FLastImprovementGeneration := CurrentGeneration;

  // Do the fitness sharing
  for i := SpeciesList.Count-1 downto 0 do
  begin
    // Retrieve the species
    Species := SpeciesList[i];

    // Reset the expected offspring count
    Species.FExpectedOffspringCount := 0;

    // Note the number of members in the species
    Species.FMemberCount := Species.Genotypes.Count;

    // Clear the AdjustedFitnessSum
    Species.FAdjustedFitnessSum := 0;
    Species.FFitnessSum := 0;

    // Reset punishment
    Species.FSpeciesPunish := 1;

    // If there's MajorStagnation going on, reduce species punish for all but
    // the best species
    if MajorStagnation and (Species.Champion<>BestGenotype) and (Species<>SecondBestSpecies) then
    begin
      // Major punishment
      Species.FSpeciesPunish := 1/1000;
    end;

    // Delete the species if it's empty!
    if (Species.Genotypes.Count=0) then
    begin
      // If any genotypes have this species as their hint, this must be cleared!
      for j := 0 to Genotypes.Count-1 do
        if Genotypes[j].SpeciesHint = Species then
          Genotypes[j].SpeciesHint := nil;

      // Signal that the species is DEAD
      Species.FDeadSpecies := true;

      // Continue with the loop
      Continue;
    end;

    // It's a live species!
    inc(FLivingSpeciesCount);

    // Calculate how much the species should be punished for age and/or
    // stagnancy

    // Is the species old?
    SpeciesOld :=
      (Species.Champion<>BestGenotype) and
      (PUNISH_SPECIES_AGE>0) and
      (Species.BirthGeneration + PUNISH_SPECIES_AGE <= CurrentGeneration);//}

    // If species is old, punish the fitness of it's genotypes!
    if SpeciesOld then
      Species.FSpeciesPunish := Species.SpeciesPunish / PUNISH_SPECIES_FACTOR;

    // Is the species stagnant?
    sp := STAGNATION_PERIOD;
      SpeciesStagnant :=
        (Species.Champion<>BestGenotype) and
        (sp>0) and ((CurrentGeneration-Species.LastImprovementGeneration)>STAGNATION_PERIOD);

    // If the species is stagnant, we must punish it's genotypes!
    if SpeciesStagnant then
      Species.FSpeciesPunish := Species.SpeciesPunish / STAGNATION_PUNISH_FACTOR;

    // Update ChampionFitnessSum
    FChampionFitnessSum := ChampionFitnessSum + Species.Champion.Fitness * Species.SpeciesPunish;

    // Update ChampionAdjustedFitnessSum
    FChampionAdjustedFitnessSum := ChampionAdjustedFitnessSum + Species.Champion.AdjustedFitness;

    // Calculate the fitness sum for each of the individuals in the species
    for j := 0 to Species.Genotypes.Count-1 do
    begin
      // Retrieve the genotype
      Genotype := Species.Genotypes[j];

      // Apply the species punishment to the genotype
      Genotype.AdjustedFitness := Genotype.AdjustedFitness * Species.SpeciesPunish;

      // Handle fitness sharing
      Genotype.AdjustedFitness := Genotype.AdjustedFitness / Species.Genotypes.Count;

      // Add the fitness to the fitnessum of the species
      Species.FFitnessSum := Species.FitnessSum + Genotype.Fitness;
      Species.FAdjustedFitnessSum := Species.AdjustedFitnessSum + Genotype.AdjustedFitness;

      // Handle the entire populations adjustedfitnesssum
      FAdjustedFitnessSum := AdjustedFitnessSum + Genotype.AdjustedFitness;
    end;
  end;

  // Calculate number of offspring per species
  for i := 0 to SpeciesList.Count-1 do
    SpeciesList[i].UpdateExpectedOffspringCount;

  // We will have faster access if the AdjustedFitnessSum s are updated,
  // though its main use was in UpdateExpectedOffspringCount
  FAdjustedFitnessSum := 0;

  // Now weed out the sub-par individuals of each species
  for i := SpeciesList.Count-1 downto 0 do
  begin
    // If the species is dead, then we need not do this at all
    if not SpeciesList[i].DeadSpecies then
    begin
      // Weed out bad individuals
      SpeciesList[i].WeedOutDeadOnes;

      // Sum the new normalized fitness sum
      FAdjustedFitnessSum := AdjustedFitnessSum + SpeciesList[i].AdjustedFitnessSum;
    end;
  end;
end;

procedure TNEATPopulation.SetMinSpeciesRelatedness(const Value: double);
begin
  FMinSpeciesRelatedness := Value;
end;

procedure TNEATPopulation.SetOnCalculateFitness(
  const Value: TOnCalculateFitness);
begin
  FOnCalculateFitness := Value;
end;

procedure TNEATPopulation.SwapPopulations;
var
  Temp : TGenotypeList;
begin
  Temp := Genotypes;
  FGenotypes := OldGeneration;
  FOldGeneration := Temp;
end;

function TNEATPopulation.PickAncestorFitProp(GenotypeList : TGenotypeList; AdjustedFitnessSum : double) : TGenotype;
var
  FitnessToLook : double;
  Genotype : TGenotype;
begin
  // Make sure that the GenotypeList isn't empty
  Assert(GenotypeList.Count>0,'GenotypeList can''t be empty!');

  // Calculate how long we should look for
  FitnessToLook := random*AdjustedFitnessSum;

  // Make sure that fitnesstolook isn't zero!
  if FitnessToLook<=0 then
    FitnessToLook := 1;

  // Guarantee a result
  Genotype := GenotypeList[0];

  // Repeat until all fitness had been "worn off"
  while FitnessToLook>0 do
  begin
    // Pick a random genotype
    Genotype := GenotypeList[random(GenotypeList.Count)];

    // Decrease the fitness we should look for
    FitnessToLook:=FitnessToLook-Genotype.AdjustedFitness;
  end;

  // Return the genotype
  result := Genotype;
end;

procedure TNEATPopulation.SetSelectionType(const Value: TSelectionType);
begin
  FSelectionType := Value;
end;

procedure TNEATPopulation.SetInputNodeCount(const Value: integer);
begin
  FInputNodeCount := Value;
end;

procedure TNEATPopulation.SetOutputNodeCount(const Value: integer);
begin
  FOutputNodeCount := Value;
end;

procedure TNEATPopulation.SetPopulationSize(const Value: integer);
begin
  FPopulationSize := Value;
end;

procedure TNEATPopulation.SetReproduceMutateRate(const Value: double);
begin
  FReproduceMutateRate := Value;
end;

procedure TNEATPopulation.ClearSplitInnovations;
var
  i : integer;
begin
  // delete all the old split innovations
  for i := 0 to FSplitInnovationList.Count-1 do
    TSplitInnovation(FSplitInnovationList[i]).Free;

  // Clear the list
  FSplitInnovationList.Clear;
end;

function TNEATPopulation.GetRandomWeight: double;
begin
  //result := Random*(1-GetMinWeight)+GetMinWeight;
  result := (random*2-1) * InitialWeightMagnitude;
end;

function TNEATPopulation.GetMinWeight: double;
begin
  result := -1;
end;

procedure TNEATPopulation.SetSpeciesTargetCount(const Value: integer);
begin
  FSpeciesTargetCount := Value;
end;

procedure TNEATPopulation.SetGenerationsToRun(const Value: integer);
begin
  FGenerationsToRun := Value;
end;

procedure TNEATPopulation.StartRun(Runs : integer; ResetFitnessMonitor : boolean);
var
  RunsSoFar : integer;
begin
  if FCurrentlyRunning then exit;
  
  try
    // Reset FStopped if it's been set
    FStopped := false;

    // Reset RunsSoFar
    RunsSoFar := 0;

    // Signal that we're currently running
    FCurrentlyRunning := true;

    // Fire the BeforeStartRun event, if there is one
    if Assigned(OnBeforeStartRun) then
      OnBeforeStartRun(self);

    // Update the fitnessmonitor stuff
    if UseFitnessMonitor then
    begin
      // Create a fitnessmonitor if one has been reqested
      CreateFitnessMonitor;

      // Reset the fitnessmonitor for a new run!
      if ResetFitnessMonitor then
        FitnessMonitor.ResetForNewRun;

      // Update the fitnessmonitor to the fact that we're now running
      FitnessMonitor.SetRunState(FCurrentlyRunning);
    end;

    // Keep running until the population is stopped
    while not FStopped and not Application.Terminated do
    begin
      // Do a double run
      DoOneRun;

      // Increase the runcounter
      inc(RunsSoFar);

      // If runssofar > Runs, then it's time to stop
      if (Runs>0) and (RunsSoFar>=Runs) then
        StopRun;
    end;
  finally
    // Signal that we're no longer running
    FCurrentlyRunning := false;

    // Update the fitnessmonitor to the fact that we're now running
    if UseFitnessMonitor then
      FitnessMonitor.SetRunState(FCurrentlyRunning);
  end;
end;

procedure TNEATPopulation.DoOneRun;
begin
  // Reset the individual counter
  FIndividualsEvaluatedThisRun := 0;

  // Lower the priority of this process!
  SetPriorityClass(GetCurrentProcess, IDLE_PRIORITY_CLASS);

  // Prepare the old fitness
  FBestFitnessOldGeneration := -1;

  // Lower the priority of this thread!
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_IDLE);

  // Reset the generation counter
  FCurrentGeneration := 0;

  // Reset SpeciesIDCounter
  FSpeciesIDCounter := 0;

  // If SpeciesTargetCount is <> 0, then set MinSpeciesRelatedness to a good
  // value (it will have been changed in the previous run)
  if SpeciesTargetCount>0 then
    MinSpeciesRelatedness := 2.2;

  // Clear out all old innovations
  DeleteAllConnectInnovations;

  // Create a the initial population
  CreatePopulation;

  // Calculate the fitness of the initial population
  CalculateFitnesses;

  // Do the rest of the generations
  while
    not FStopped and
    not Application.Terminated and
    (CurrentGeneration<GenerationsToRun) and
    not WinnerFound do
  begin
    // Increase the generation counter
    inc(FCurrentGeneration);

    // Store the old fitness
    FBestFitnessOldGeneration := BestGenotype.Fitness;

    // Create a new population
    CreateNewGeneration;

    // Calculate the fitness of the new generation
    CalculateFitnesses;
  end;
end;

procedure TNEATPopulation.StopRun;
begin
  FStopped := true;

  // Signal that we're no longer running
  FCurrentlyRunning := false;

  // Tell the fitnessmonitor that the run's about to start?
  if UseFitnessMonitor and Assigned(FitnessMonitor) then
    FitnessMonitor.SetRunState(FCurrentlyRunning);

  // Fire the OnAfterRunStopped event
  if Assigned(OnAfterRunStopped) then
    OnAfterRunStopped(self);
end;

function TNEATPopulation.WinnerFound: boolean;
begin
  result := Assigned(BestGenotype) and (BestGenotype.Fitness>=FTargetFitness);
end;

procedure TNEATPopulation.SetTargetFitness(const Value: double);
begin
  FTargetFitness := Value;
end;

procedure TNEATPopulation.SetOnShowBestIndividual(
  const Value: TOnShowBestIndividual);
begin
  FOnShowBestIndividual := Value;
end;

procedure TNEATPopulation.ShowBestIndividual;
begin
  // Make sure that there is a best individual assigned!
  Assert(Assigned(BestGenotype),'BestIndividual not assigned!');

  // If FOnShowBestIndividual is assigned, fire it!
  if Assigned(FOnShowBestIndividual) then
    FOnShowBestIndividual(self, BestGenotype, FBestFitnessOldGeneration<>BestGenotype.Fitness);

  // Give some control back to windows
  Application.ProcessMessages;

  // Has the run been terminated?
  if FStopped then
    exit;
end;

procedure TNEATPopulation.Loaded;
begin
  inherited;
  // If it's set to autostart, this is the time to start it up!
end;

procedure TNEATPopulation.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  // Stop evolving if our parent is deleted!
  if (AComponent = Owner) and (Operation=opRemove) then
    StopRun;
end;

function TNEATPopulation.GetAverageFitness: double;
begin
  // Avoid div by zero!
  if Genotypes.Count=0 then
  begin
    result := 0;
    exit;
  end;

  // Calculate the average fitness
  result := FitnessSum/Genotypes.Count;
end;

function TNEATPopulation.GetAverageSize: double;
var
  i : integer;
  SizeSum : integer;
begin
  // Avoid div by zero!
  if Genotypes.Count=0 then
  begin
    result := 0;
    exit;
  end;

  // Reset the sizesum
  SizeSum := 0;

  // Sum up all the sizes
  for i := 0 to Genotypes.Count - 1 do
    SizeSum := SizeSum + Genotypes[i].GetSize;

  // Calculate the average size
  result := SizeSum / Genotypes.Count
end;

procedure TNEATPopulation.SetUseFitnessMonitor(const Value: boolean);
begin
  FUseFitnessMonitor := Value;
end;

procedure TNEATPopulation.SetFitnessMonitor(
  const Value: TfrmFitnessMonitorBase);
begin
  FFitnessMonitor := Value;
end;

procedure TNEATPopulation.CreateFitnessMonitor;
begin
  if UseFitnessMonitor and (FitnessMonitor=nil) then
  begin
    // Create the fitness monitor
    FFitnessMonitor := TfrmFitnessMonitor.Create(self);

    // Assign self as the population for the fitness monitor
    FitnessMonitor.NEATPopulation := self;

    // Show the fitness monitor
    FitnessMonitor.Show;

    // Use target fitness as the max fitness value
    FitnessMonitor.SetBestFitness(TargetFitness);
  end;
end;

procedure TNEATPopulation.DeleteAllConnectInnovations;
{$if CONNECTION_INNOV_MEM>0}
var
  i  : integer;
{$ifend}
begin
  // No memory, delete them all!
  {$if CONNECTION_INNOV_MEM>0}
  for i := 0 to FGenerationInnovationList.Count - 1 do
    FGenerationInnovationList[i].Free;
  {$ifend}

  // Clear the list
  FGenerationInnovationList.Clear;
end;

function TNEATPopulation.TournamentSelectSpecies(
  SpeciesCount: integer): TSpecies;
var
  i : integer;
  Species, BestSpecies : TSpecies;
begin
  BestSpecies := SpeciesList[0];

  // Try SpeciesCount random species
  for i := 0 to SpeciesCount-1 do
  begin
    // Pick a random species
    Species := SpeciesList[Random(SpeciesList.Count)];

    // Is it the best species so far?
    if (i=0) or (Species.FitnessSum>BestSpecies.FitnessSum) then
      BestSpecies := Species;
  end;

  // Return the best species
  result := BestSpecies;
end;

procedure TNEATPopulation.SetOnBeforeStartRun(const Value: TNotifyEvent);
begin
  FOnBeforeStartRun := Value;
end;

procedure TNEATPopulation.SetOnAfterRunStopped(const Value: TNotifyEvent);
begin
  FOnAfterRunStopped := Value;
end;

procedure TNEATPopulation.SetTournamentRange(const Value: integer);
begin
  FTournamentRange := Value;
end;

procedure TNEATPopulation.SetTournamentSize(const Value: integer);
begin
  FTournamentSize := Value;
end;

procedure TNEATPopulation.SetConnectionWeightMutationChance(
  const Value: double);
begin
  FConnectionWeightMutationChance := Value;
end;

procedure TNEATPopulation.SetConnectionMutationBigChangeChance(
  const Value: double);
begin
  FConnectionMutationBigChangeChance := Value;
end;

procedure TNEATPopulation.SetMutateCrossed(const Value: boolean);
begin
  FMutateCrossed := Value;
end;

procedure TNEATPopulation.SetConnectNodesOfInitialPop(const Value: boolean);
begin
  FConnectNodesOfInitialPop := Value;
end;

procedure TNEATPopulation.SetConnectOutputOfInitialPop(const Value: boolean);
begin
  FConnectOutputOfInitialPop := Value;
end;

procedure TNEATPopulation.SetActivationIterations(const Value: integer);
begin
  FActivationIterations := Value;
end;

procedure TNEATPopulation.SetAllowRecurrentLinks(const Value: boolean);
begin
  FAllowRecurrentLinks := Value;
end;

function TNEATPopulation.PickSpeciesFitnessProp: TSpecies;
var
  FitnessToLook : double;
  Species : TSpecies;
  AllowedSpecies : boolean;
begin
  // Calculate how long we should look for
  FitnessToLook := random*AdjustedFitnessSum;

  // Make sure that fitnesstolook isn't zero!
  if FitnessToLook<=0 then
    FitnessToLook := 0.001;

  AllowedSpecies := false;

  Species := SpeciesList[0];

  // Repeat until all fitness had been "worn off"
  while (FitnessToLook>0) or not AllowedSpecies do
  begin
    // Pick a random Species
    Species := SpeciesList[random(SpeciesList.Count)];

    // Decrease the fitness we should look for
    FitnessToLook:=FitnessToLook-Species.AdjustedFitnessSum;

    // Allowed
    AllowedSpecies :=
       not (Species.DeadSpecies) and (Species.Genotypes.Count>0);
  end;

  // Return the species
  result := Species;
end;

procedure TNEATPopulation.AdjustMinSpeciesRelatedness;
var
  SpeciesDelta : double;
  SpeciesStep : double;

begin
  // Should any adjustments be made?
  if SpeciesTargetCount>0 then
  begin
    SpeciesDelta := Abs(FLivingSpeciesCount-SpeciesTargetCount);

    // Ignore a small difference in species count
    if SpeciesDelta<=2 then
      exit;

    if SpeciesDelta>5 then
      SpeciesStep := MIN_SPECIES_RELATEDNESS_DELTA * 10
    else
      SpeciesStep := MIN_SPECIES_RELATEDNESS_DELTA;

    if SpeciesDelta>10 then
      SpeciesStep := SpeciesDelta / 4;

    // If there are too many species, reduce min species relatedness
    if (FLivingSpeciesCount>SpeciesTargetCount) then
      MinSpeciesRelatedness := MinSpeciesRelatedness + SpeciesStep;

    // If there are too few species, reduce min species relatedness
    if (FLivingSpeciesCount<SpeciesTargetCount) then
      MinSpeciesRelatedness := MinSpeciesRelatedness - SpeciesStep;

    MinSpeciesRelatedness := Min(15, Max(0.4, MinSpeciesRelatedness));
  end;//}
end;

procedure TNEATPopulation.SetSurvivalThreshold(const Value: double);
begin
  Assert((Value>0) and (Value<1),'SurvivalThreshold must be between 0 and 1');
  FSurvivalThreshold := Value;
end;

procedure TNEATPopulation.SetSpeciesFitnessMethod(
  const Value: TSpeciesFitnessMethod);
begin
  FSpeciesFitnessMethod := Value;
end;

procedure TNEATPopulation.SetOnPrepareInitialGenotype(
  const Value: TOnPrepareInitialGenotype);
begin
  FOnPrepareInitialGenotype := Value;
end;

procedure TNEATPopulation.SetOnGenotypeIteration(
  const Value: TOnGenotypeIteration);
begin
  FOnGenotypeIteration := Value;
end;

procedure TNEATPopulation.SetGenotypeIterationEvent(const Value: boolean);
begin
  FGenotypeIterationEvent := Value;
end;

procedure TNEATPopulation.SetBestFitnessOldGeneration(const Value: double);
begin
  FBestFitnessOldGeneration := Value;
end;

procedure TNEATPopulation.SetConnectionAddChance(const Value: double);
begin
  FConnectionAddChance := Value;
end;

procedure TNEATPopulation.SetConnectionSplitChance(const Value: double);
begin
  FConnectionSplitChance := Value;
end;

procedure TNEATPopulation.SetWeightUnrelatednessFactor(const Value: single);
begin
  FWeightUnrelatednessFactor := Value;
end;

procedure TNEATPopulation.SetWeightPeturbationFactor(const Value: single);
begin
  FWeightPeturbationFactor := Value;
end;

procedure TNEATPopulation.SetInitialWeightMagnitude(const Value: single);
begin
  FInitialWeightMagnitude := Value;
end;

function TNEATPopulation.GetRandomConnectionDelay: integer;
var
  r : single;
begin
  {r := (random+random+random)/3; // Value centered around 0.5
  r := abs(r-0.5)*2; // Value from 0 and up
  r := min(1, max(0, r)); // Value guaranteed in the range 0..1//}
  r := random;

  result := round(r * MaxConnectionDelay);
end;

{ TConnect }

function TConnect.CheckConnectValidity: boolean;
begin
  result :=
    (InnovationID>=0) and (InnovationID<Genotype.NEATPopulation.ConnectInnovationCounter) and
    (TailNodeID>=0) and (TailNodeID<Genotype.NEATPopulation.NodeInnovationCounter) and
    (HeadNodeID>=0) and (HeadNodeID<Genotype.NEATPopulation.NodeInnovationCounter);
end;

procedure TConnect.ClearDelayedValueArray;
var
  i : integer;
begin
  for i := 0 to High(FDelayedValueArray) do
    FDelayedValueArray[i] := 0;
end;

procedure TConnect.Clear;
begin
  FTouchOfDeath := false;
  FDelay := 0;
  ClearDelayedValueArray;
end;

function TConnect.Copy: TConnect;
var
  NewConnect : TConnect;
begin
  // Get or create a new connect
  NewConnect := CacheHandler.GetOrCreateConnect(Genotype);

  // Copy this connect to the new connect
  CopyTo(NewConnect);

  // Return the new connect
  result := NewConnect;
end;

procedure TConnect.CopyTo(Connect: TConnect);
begin
  Connect.FInnovationID := InnovationID;
  Connect.FTailNodeID := TailNodeID;
  Connect.FHeadNodeID := HeadNodeID;
  Connect.FWeight := Weight;
  Connect.FEnabled := Enabled;
  Connect.FDelay := Delay;
end;

constructor TConnect.Create(Genotype: TGenotype);
begin
  self.FGenotype := Genotype;
  FWeight := 1;
  MutateWeight;

  inc(G_ConnectCreated);
end;

destructor TConnect.Destroy;
begin
  inc(G_ConnectDestroyed);

  inherited;
end;

procedure TConnect.FlipEnabled;
begin
  FEnabled := not Enabled;
end;

function TConnect.GetValue: double;
var
  Val  : double;
  i : integer;
begin
  Val := TailNode.Value * Weight;

  if Delay = 0 then
    result := Val
  else
  begin
    result := FDelayedValueArray[0];

    for i := 0 to Delay-1 do
      FDelayedValueArray[i] := FDelayedValueArray[i+1];

    FDelayedValueArray[Delay] := Val;
  end;
end;

function TConnect.IdenticalTo(Connect: TConnect): boolean;
begin
  // If head and tail nodes are identical, then the connection is considered to
  // be identincal too
  result :=
    (TailNodeID = Connect.TailNodeID) and
    (HeadNodeID = Connect.HeadNodeID) and
    (Delay = Connect.Delay);
end;

function TConnect.IsRecurrent: boolean;
begin
  // This calculates if the node is recurrent
  result := Genotype.GetNodeByID(HeadNodeID).YPos <= Genotype.GetNodeByID(TailNodeID).YPos
end;

function TConnect.MutateSplit : TNode;
var
  Node : TNode;
  TempNode : TNode;
  Connect1, Connect2 : TConnect;
  i : integer;
  SplitInnovation : TSplitInnovation;
begin
  result := nil;

  // Make sure that the connection is valid
  Assert(CheckConnectValidity, 'MutateSplit failed, connections is allready invalid!');

  // If the connection is allready disabled, this won't do much good
  if not Enabled then
    exit;//}

  // Clear the SplitInnovation link
  SplitInnovation := nil;

  // See if there is allready such a split!?
  for i := 0 to Genotype.NEATPopulation.FSplitInnovationList.Count-1 do
  begin
    // Retrieve the splitinnovation
    SplitInnovation := TSplitInnovation(Genotype.NEATPopulation.FSplitInnovationList[i]);

    // Is it a split of this connection?
    if SplitInnovation.InnovationID = InnovationID then
    begin
      // Increase the count by one
      inc(SplitInnovation.FCount);

      // It is! Use this split
      break;
    end;
  end;  //}

  // If we didn't find a prior split innovation, we must create a new one!
  if (SplitInnovation=nil) or (SplitInnovation.InnovationID <> InnovationID) then
  begin
    // Create the new split innovation
    SplitInnovation := TSplitInnovation.Create;

    // Add it to the list
    Genotype.NEATPopulation.FSplitInnovationList.Add(SplitInnovation);

    // Setup all values
    SplitInnovation.FInnovationID := InnovationID;

    // Set the usage count to 0
    SplitInnovation.FCount := 1;

    // Get the new innovations!
    SplitInnovation.FNewNodeID := Genotype.NEATPopulation.GetNextNodeInnovation(nil);
    SplitInnovation.FFirstInnovationID := Genotype.NEATPopulation.GetNextConnectInnovation(nil);
    SplitInnovation.FSecondInnovationID := Genotype.NEATPopulation.GetNextConnectInnovation(nil);
  end;

  // Maker sure that there is no node, or connects, allready in the genotype
  // with these numbers! (never failed yet)
{  Assert(Genotype.GetNodeByID(SplitInnovation.NewNodeID)=nil,'NodeID allready exists!');
  Assert(Genotype.GetConnectByID(SplitInnovation.FirstInnovationID)=nil,'FirstInnovationID allready exists!');
  Assert(Genotype.GetConnectByID(SplitInnovation.SecondInnovationID)=nil,'SecondInnovationID allready exists!');//}

  // Create a new node and two new links
  Node := CacheHandler.GetOrCreateNode(Genotype);
  result := Node;
  Node.MutateFunction;
  Node.NodeID := SplitInnovation.NewNodeID;

  // Assign a  higher priority to the new node, so that it will be evaluated
  // BEFORE the old node
  // Get the tailnode
  TempNode := Genotype.GetNodeByID(TailNodeID);

  // If there is such a node in the genome (there needn't be), set the prio to
  // one above the temp node
  if TempNode <> nil then
    Node.Priority := TempNode.Priority+1
  else
    // If there is no such node, arbitrarity give it a prio of 1
    Node.Priority := 1;

  // Add the new node to the genotype
  Genotype.NodeList.Add(Node);

  // Connect link from tail to new node
  Connect1 := CacheHandler.GetOrCreateConnect(Genotype);
  Connect1.FTailNodeID := TailNodeID;
  // WEIGHT IS SET LATER
  Connect1.FHeadNodeID := Node.NodeID;
  Connect1.FEnabled := true;
  Connect1.FInnovationID := SplitInnovation.FirstInnovationID;

  if random<0.8 then
    Connect1.FDelay := FDelay // Use the parent connect delay
  else
    Connect1.FDelay := Genotype.NEATPopulation.GetRandomConnectionDelay; // Pick a new delay

  Genotype.ConnectList.Add(Connect1);

  // Connect link from new node to head
  Connect2 := CacheHandler.GetOrCreateConnect(Genotype);
  Connect2.FTailNodeID := Node.NodeID;
  // WEIGHT IS SET LATER
  Connect2.FHeadNodeID := HeadNodeID;
  Connect2.FEnabled := true;
  Connect2.FInnovationID := SplitInnovation.SecondInnovationID;
  Connect2.FDelay := Genotype.NEATPopulation.GetRandomConnectionDelay; // Pick a new delay
  Genotype.ConnectList.Add(Connect2);

  // The YPos of the new node is (from.YPos + to.YPos)/2
  Node.YPos := (Genotype.GetNodeByID(HeadNodeID).YPos + Genotype.GetNodeByID(TailNodeID).YPos)/2;

  {$ifdef WEIGHT_FIRST}
    Connect1.FWeight := Weight;
    Connect2.FWeight := 1;
  {$else}
    Connect1.FWeight := 1;
    Connect2.FWeight := Weight;
  {$endif}

  // Make sure that all the connections and nodes are valid!
  Assert(Connect1.CheckConnectValidity, 'MutateSplit failed, Connect1 is invalid!');
  Assert(Connect2.CheckConnectValidity, 'MutateSplit failed, Connect2 is invalid!');
  Assert(Node.CheckNodeValidity, 'MutateSplit failed, Node is invalid!');

  // Make sure they were properly added (never failed yet)
{  Assert(Genotype.ConnectList.IndexOf(Connect1)>-1,'Connect 1 not in list!');
  Assert(Genotype.ConnectList.IndexOf(Connect2)>-1,'Connect 2 not in list!');
  Assert(Genotype.NodeList.IndexOf(Node)>-1,'Node not in list!');//}

  // If the depth of the new node doesn't allready exist in the genotype, then
  // the maxnodedepth has increased by one!
  for i := 0 to Genotype.NodeList.Count-1 do
  begin
    // That's the node we just added, (it should be last)
    if Genotype.NodeList[i] = Node then
    begin
      // We didn't find another node with the same depth, otherwise we wouldn't
      // have come here. That means that the depth should be increased
      Genotype.MaxNodeDepth := Genotype.MaxNodeDepth + 1; 
    end;

    // If the node has the same YPos as the new node, then the depth hasn't
    // increased
    if Genotype.NodeList[i].YPos = Node.YPos then
      break;
  end;

  // Disable this connect, since it's to be used no more
  FEnabled := false;
end;

procedure TConnect.MutateWeight;
var
  rnd : double;
begin
  // No point in mutating disabled connections
  if not Enabled then exit;

  if random<=Genotype.NEATPopulation.ConnectionMutationBigChangeChance then
    FWeight := Genotype.NEATPopulation.GetRandomWeight
  else//}
  begin
    RND := random; // Non gaussian
    //RND := (random+random+random)/3; // Gaussian
    //RND := random-random;
    FWeight := Weight + (rnd*2-1)*Genotype.NEATPopulation.WeightPeturbationFactor;
  end;

  // Limit the weight to the FMaxLinkWeight factor
  if FWeight > Genotype.NEATPopulation.FMaxLinkWeight then
    FWeight := Genotype.NEATPopulation.FMaxLinkWeight

  else if FWeight < -Genotype.NEATPopulation.FMaxLinkWeight then
    FWeight := -Genotype.NEATPopulation.FMaxLinkWeight;
end;

procedure TConnect.Reverse;
var
  temp : integer;
begin
  temp := HeadNodeID;
  FHeadNodeID := TailNodeID;
  FTailNodeID := temp;
end;

procedure TConnect.SetDelay(const Value: integer);
begin
  FDelay := Max(0, Min(Value, High(FDelayedValueArray)));;
end;

procedure TConnect.Flush;
begin
  ClearDelayedValueArray;
end;

{ TNode }

function TNode.CalculateValue: double;
begin
  // Make sure that the node has a transfer function!
  Assert(Assigned(TransferFunction),'The node has no transfer function assigned!');

  // Use the transfer function to get the value
  result := TransferFunction.CalculateValue(self);
end;

function TNode.CheckNodeValidity: boolean;
begin
  result :=
    (NodeID>-1) and
    (NodeID<Genotype.NEATPopulation.NodeInnovationCounter);
end;

procedure TNode.Clear;
begin
  NodeID := -1;
  Value := 0;
  OldValue := 0;
  ActivationCount := 0;
  FActivationSum := 0;
  FTouchOfDeath := false;
  EnabledConnectList.Clear;
  IsInputNode := false;
  IsOutputNode := false;
  BiasNode := false;
  YPos := -1;
  ClearComingValue;
end;

procedure TNode.ClearComingValue;
var
  i : integer;
begin
  // These values are used for delayed transfer functions
  for i := 0 to High(ComingValue) do
    ComingValue[i] := 0;
end;

function TNode.Copy: TNode;
var
  NewNode : TNode;
begin
  // Get a node from the cache, or create a new one
  NewNode := CacheHandler.GetOrCreateNode(Genotype);

  // Copy to the new node
  CopyTo(NewNode);

  // Return the new node
  result := NewNode;
end;

procedure TNode.CopyTo(Node: TNode);
begin
  Node.NodeID := NodeID;
  Node.IsInputNode := IsInputNode;
  Node.IsOutputNode := IsOutputNode;
  Node.TransferFunction := TransferFunction;
  Node.Value := 0;
  Node.Genotype := Genotype;
  Node.Priority := Priority;
  Node.YPos := YPos;
  Node.BiasNode := BiasNode;
end;

constructor TNode.Create(Genotype : TGenotype);
begin
  // Make sure that the genotype is valid
  Assert(Assigned(Genotype),'No genotype supplied!');

  // If a nodeId turns up -1, that means that the program is broken somewhere!
  NodeID := -1;

  FEnabledConnectList := TConnectList.Create;

  Self.Genotype := Genotype;

  // Assume 0 for value
  Value := 0;
  ActivationCount := 0;
  OldValue := 0;
  Priority := 0;

  // Pick a transfer function!
  TransferFunction := Genotype.NEATPopulation.GetRandomTransferFunction;

  inc(G_NodeCreated);
end;

destructor TNode.Destroy;
begin
  Clear;

  FreeAndNil(FEnabledConnectList);

  inc(G_NodeDestroyed);

  inherited;
end;

procedure TNode.Flush;
begin
  if not FIsInputNode then
  begin
    FValue := 0;
    FOldValue := 0;

    ClearComingValue;
  end;

  FActivationCount := 0;
end;

function TNode.GetValue: double;
begin
  result := FValue
{  if ActivationCount>0 then
    result := FValue
  else
    result := 0;//}
end;

procedure TNode.MutateFunction;
begin
  // Ask the population for a new approved transfer function
  TransferFunction := Genotype.NEATPopulation.GetRandomTransferFunction;
end;

function TNode.SaveToString: string;
var
  temps : string;
begin
  if IsInputNode then
    temps := Format('Input (Value=%f)',[Value])
  else
  begin
   {$ifdef USE_PRIORITY}
    temps := format('%s, Value=%f, Prio:%d',[TransferFunction.TransferFunctionName, Value, Priority]);
   {$else}
    temps := format('%s, Value=%f',[TransferFunction.TransferFunctionName, Value]);
   {$endif}

    if IsOutputNode then
      Temps := format('Output (%s)',[temps])
    else
      Temps := format('Hidden (%s)',[temps]);
  end;

  result := Format('N %d : %s',[NodeID,temps]);
end;

procedure TNode.SetValue(const Value: double);
begin
  // OldValue is set after the entire iteration is done!
  //OldValue := FValue;
  inc(FActivationCount);

  FValue := Value;

  if IsInputNode then
    OldValue := FValue;//}

{  if IsOutputNode then
    OldValue := FValue;//}
end;

{ TTransferFunction }

function TTransferFunction.CalculateValue(Node: TNode): double;
begin
  result := 0;
  Assert(false,Format('%s must override CalculateValue!',[ClassName]));
end;

function TTransferFunction.GetMul(Node: TNode): double;
var
  i : integer;
  M : double;
  Connect : TConnect;
begin
  // Reset the multiplier
  M := 1;

  // For each enabled connect, multiply it's value onto the multiplier
  for i := 0 to Node.EnabledConnectList.Count-1 do
  begin
    Connect := Node.EnabledConnectList[i];

    if Connect.Enabled then
    begin
      M := M * Connect.GetValue;
      if abs(M)>10e8 then
        M := 0;
    end;

    if M=0 then
      break;
  end;

  // Keep the activation multiplier
  Node.ActivationSum := M;

  result := M;
end;

function TTransferFunction.GetSum(Node: TNode): double;
var
  i : integer;
  sum : double;
  Connect : TConnect;
begin
  // Reset the sum
  sum := 0;

  // Sum all the values
  for i := 0 to Node.EnabledConnectList.Count-1 do
  begin
    // Retrieve the connect
    Connect := Node.EnabledConnectList[i];

    // It MUST be neabled
    Assert(Connect.Enabled, 'Connection is not enabled!');

    // Add the sum
    sum := sum + Connect.GetValue;
  end;

  // Keep the activation sum
  Node.ActivationSum := Sum;

  // Return the sum
  result := Sum;
end;

procedure TTransferFunction.SetTransferFunctionName(const Value: string);
begin
  FTransferFunctionName := Value;
end;

{ TSpecies }

procedure TSpecies.Add(Genotype: TGenotype);
begin
  // Add the genotype to the list of genotypes
  Genotypes.Add(Genotype);

  // If it's of better fitness, or equal fitness but smaller, it's the champ
  if (Champion=nil) or
     (Champion.Fitness<Genotype.Fitness) or
    ((Champion.Fitness=Genotype.Fitness) and (Genotype.GetSize<Champion.GetSize)) then
    FChampion := Genotype;
end;

procedure TSpecies.CalculateAdjustedFitnessSum;
var
  i : integer;
begin
  // Clear the AdjustedFitnessSum
  FAdjustedFitnessSum := 0;

  // Sum them up!
  for i := 0 to Genotypes.Count-1 do
    FAdjustedFitnessSum := AdjustedFitnessSum + Genotypes[i].AdjustedFitness;

  // Make sure that the adjusted fitness isn't 0!
  //Assert(AdjustedFitness>0, 'No fitness in species!');
  
  // Make sure that the species isn't empty
  Assert(Genotypes.Count>0,'Species is emtpy');
end;

constructor TSpecies.Create(ANEATPopulation : TNEATPopulation);
begin
  FGenotypes := TGenotypeList.Create;
  FNEATPopulation := ANEATPopulation;

  // Create a random color for the species - this is used for visualization
  // purposes
  FColor := 1 shl 24;
  FColor := Random(Color);

  inc(G_SpeciesCreated);
end;

destructor TSpecies.Destroy;
begin
  if DefiningMember<>nil then
  begin
    DefiningMember.CacheNodesAndConnects;
    FreeAndNil(FDefiningMember);
  end;

  FreeAndNil(FGenotypes);

  inc(G_SpeciesDestroyed);
  inherited;
end;

function TSpecies.FitnessPropSelection: TGenotype;
begin
  result := FNEATPopulation.PickAncestorFitProp(Genotypes, AdjustedFitnessSum);
end;

function FitnessSortCompare(Item1, Item2: Pointer): Integer;
var
  Genotype1, Genotype2 : TGenotype;
begin
  Genotype1 := TGenotype(Item1);
  Genotype2 := TGenotype(Item2);

  // Sort the highest fitness first
  if Genotype1.Fitness>Genotype2.Fitness then
    result := -1
  else if Genotype1.Fitness=Genotype2.Fitness then
    result := 0
  else
    result := 1;
end;

function TSpecies.GetRandomGenotype: TGenotype;
begin
  result := Genotypes[random(Genotypes.Count)];
end;

function TSpecies.TournamentSelection(GenotypesToTry: integer): TGenotype;
var
  i : integer;
  BestSoFar, Genotype : TGenotype;
begin
  // Pick the first genotype
  BestSoFar := GetRandomGenotype;

  // Make sure we don't search more than the entire species
  GenotypesToTry := min(GenotypesToTry, Genotypes.Count);

  // Try as many times as ordered (one is allready tested!)
  for i := 0 to GenotypesToTry-2 do
  begin
    // Pick a random genotype
    Genotype := GetRandomGenotype;

    // If this genotype is better than the previous genotype, we should use it
    // instead
    if Genotype.Fitness>BestSoFar.Fitness then
      BestSoFar := Genotype;
  end;

  // Return the best genotype
  result := BestSoFar;
end;

procedure TSpecies.UpdateExpectedOffspringCount;
var
  i : integer;
  AverageAdjustedFitness : double;
  Genotype : TGenotype;
  ToSpawn, ToSpawnSum : double;
begin
  // Start with zero expected offspring
  ToSpawnSum := 0;
  FExpectedOffspringCount := 0;

  // If it's a dead or empty species, just get out
  if DeadSpecies or (Genotypes.Count=0) then
    exit;

  if FNEATPopulation.SpeciesFitnessMethod = sfmAverageFitness then
  begin
    // Compute the average fitness in the population
    AverageAdjustedFitness := (FNEATPopulation.AdjustedFitnessSum / FNEATPopulation.PopulationSize);

    // Sum every genotypes expected offspring
    for i := 0 to Genotypes.Count-1 do
    begin
      // Retrieve genotype for easy access
      Genotype := Genotypes[i];

      // This genomes expected offspring
      ToSpawn := Genotype.AdjustedFitness / AverageAdjustedFitness;
      Genotype.FExpectedOffspring := trunc(ToSpawn);

      // Increase the number of expected offspring
      ToSpawnSum := ToSpawnSum + ToSpawn;
    end;//}
  end else
  if FNEATPopulation.SpeciesFitnessMethod = sfmHighestFitness then
  begin

    Assert(SpeciesPunish>0, 'SpeciesPunish can''t be zero.');

    // A simpler version - let the fitness of the champion decide how many
    // individuals each species should have. This method appears slightly worse
    // than the standard version.
    ToSpawnSum := (Champion.Fitness * SpeciesPunish / FNEATPopulation.ChampionFitnessSum) * FNEATPopulation.PopulationSize;
  end else
    Assert(false, 'This species fitness method hasn''t been implemented yet!');

  // Set the number of expected offspring
  FExpectedOffspringCount := Trunc(ToSpawnSum);

  // I don't like tiny species
  {if (ExpectedOffspringCount<=1) and (Champion<>NEATPopulation.BestGenotype) then
    ExpectedOffspringCount := 0;//}

  // Make sure that ExpectedOffspringCount isn't too high
  Assert(ExpectedOffspringCount<=FNEATPopulation.PopulationSize,
    Format('ExpectedOffspringCount is too large, %d, max allowed is %d.',
      [ExpectedOffspringCount,FNEATPopulation.PopulationSize]));
end;

procedure TSpecies.WeedOutDeadOnes;
var
  SurvivalCount : integer;
begin
  // Sort the genotypes
  Genotypes.Sort(FitnessSortCompare);

  // How many are to survive at all? (min 1)
  SurvivalCount := ceil(max(1, (FNEATPopulation.SurvivalThreshold * Genotypes.Count)));

  // Remove the last one until only SurvivalCount remain
  while Genotypes.Count>SurvivalCount do
    Genotypes.Delete(Genotypes.Count-1);

  // Make sure that the species isn't empty
  Assert(Genotypes.Count>0,'Species is emtpy');

  // Recalculate the AdjustedFitnessSum of the Species
  CalculateAdjustedFitnessSum;
end;

{ TSpeciesList }

function TSpeciesList.GetItems(i: integer): TSpecies;
begin
  result := TSpecies(Get(i));
end;

procedure TSpeciesList.SetItems(i: integer; const Value: TSpecies);
begin
  Put(i, Value);
end;

{ TfrmFitnessMonitorBase }

procedure TfrmFitnessMonitorBase.PublishGenotype(Genotype : TGenotype);
begin
  // NADA
end;

procedure TfrmFitnessMonitorBase.ResetForNewRun;
begin
  // NADA
end;

procedure TfrmFitnessMonitorBase.SetBestFitness(const Value: double);
begin
  // NADA
end;

procedure TfrmFitnessMonitorBase.SetRunState(Running: boolean);
begin
  // NADA
end;

procedure TfrmFitnessMonitorBase.UpdatePopulationView;
begin
  // NADA
end;

{ TCacheHandler }

procedure TCacheHandler.AddConnectToCache(Connect: TConnect);
begin
  Assert(Assigned(Connect),'Can''t cache a nil connect!');

  // Clear the connect - this prepares it for caching
  Connect.Clear;

  // Add it to the cache
  FConnectCache.Add(Connect);
end;

procedure TCacheHandler.AddNodeToCache(Node: TNode);
begin
  Assert(Assigned(Node),'Can''t cache a nil node!');

  // Clear the node - this prepares it for caching
  Node.Clear;

  // Add it to the cache
  FNodeCache.Add(Node);
end;

procedure TCacheHandler.ClearCache;
var
  i : integer;
begin
  // Clear the node cache
  for i := 0 to FNodeCache.Count-1 do
    FNodeCache[i].Free;

  FNodeCache.Clear;

  // Clear the connect cache
  for i := 0 to FConnectCache.Count-1 do
    FConnectCache[i].Free;

  FConnectCache.Clear;
end;

constructor TCacheHandler.Create;
begin
  // Create the node cache
  FNodeCache := TNodeList.Create(false);
  FNodeCache.Duplicates := dupAccept;

  // Create the connect cache
  FConnectCache := TConnectList.Create(false);
  FConnectCache.Duplicates := dupAccept;
end;

destructor TCacheHandler.Destroy;
begin
  // Clear the cache
  ClearCache;

  // Destroy the two lists
  FreeAndNil(FNodeCache);
  FreeAndNil(FConnectCache);

  inherited;
end;

function TCacheHandler.GetOrCreateConnect(Genotype: TGenotype): TConnect;
begin
  // Is there a connect in the cache?
  if FConnectCache.Count>0 then
  begin
    // Retrieve the cached connect
    result := FConnectCache[FConnectCache.Count-1];

    // Remove it from the cache
    FConnectCache.Delete(FConnectCache.Count-1);

    // Set the genotype
    result.FGenotype := Genotype;
  end else
    // Create a new connect
    result := TConnect.Create(Genotype);

  Assert(Assigned(result),'Failed to retrieve a Connect!');
end;

function TCacheHandler.GetOrCreateNode(Genotype: TGenotype): TNode;
var
  NewNode : TNode;
begin
  // Is there a node in the cache?
  if FNodeCache.Count>0 then
  begin
    // Retrieve the cached node
    NewNode := FNodeCache[FNodeCache.Count-1];

    // Remove it from the cache
    FNodeCache.Delete(FNodeCache.Count-1);

    // Set the genotype
    NewNode.Genotype := Genotype;
  end else
    // Create a new node
    NewNode := TNode.Create(Genotype);

  // Return the new node
  result := NewNode;

  Assert(Assigned(result),'Failed to retrieve a node!');
end;

initialization
  CacheHandler := TCacheHandler.Create;

  RegisterNewTransferFuntion('Main','Sigmoid',TTransferFunctionSigmoid.Create);

finalization
  FreeAndNil(CacheHandler);
end.

