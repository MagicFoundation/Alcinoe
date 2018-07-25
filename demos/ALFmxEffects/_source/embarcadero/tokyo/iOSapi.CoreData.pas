{**********************************************************}
{                                                          }
{           CodeGear Delphi Runtime Library                }
{                                                          }
{ Delphi-Objective-C Bridge                                }
{ Interfaces for Cocoa framework CoreData                  }
{                                                          }
{ Copyright (c) 2004-2011, Apple Inc. All rights reserved. }
{                                                          }
{ Translator: Embarcadero Technologies, Inc.               }
{   Copyright(c) 2012-2017 Embarcadero Technologies, Inc.  }
{              All rights reserved                         }
{                                                          }
{**********************************************************}

unit iOSapi.CoreData;

interface

uses
  Macapi.ObjectiveC, iOSapi.CocoaTypes, iOSapi.Foundation;

const
  NSAddEntityMappingType = 2;
  NSBinaryDataAttributeType = 1000;
  NSBooleanAttributeType = 800;
  NSCascadeDeleteRule = 2;
  NSConfinementConcurrencyType = 0;
  NSCopyEntityMappingType = 4;
  NSCoreDataError = 134060;
  NSCoreDataVersionNumber10_4 = 46.0;
  NSCoreDataVersionNumber10_4_3 = 77.0;
  NSCoreDataVersionNumber10_5 = 185.0;
  NSCoreDataVersionNumber10_5_3 = 186.0;
  NSCoreDataVersionNumber10_6 = 246.0;
  NSCoreDataVersionNumber10_6_2 = 250.0;
  NSCoreDataVersionNumber10_6_3 = 251.0;
  NSCoreDataVersionNumber_iPhoneOS_3_0 = 241.0;
  NSCoreDataVersionNumber_iPhoneOS_3_1 = 248.0;
  NSCoreDataVersionNumber_iPhoneOS_3_2 = 310.2;
  NSCoreDataVersionNumber_iPhoneOS_4_0 = 320.5;
  NSCoreDataVersionNumber_iPhoneOS_4_1 = 320.11;
  NSCoreDataVersionNumber_iPhoneOS_4_2 = 320.15;
  NSCoreDataVersionNumber_iPhoneOS_4_3 = 320.17;
  NSCountResultType = 4;
  NSCustomEntityMappingType = 1;
  NSDateAttributeType = 900;
  NSDecimalAttributeType = 400;
  NSDenyDeleteRule = 3;
  NSDictionaryResultType = 2;
  NSDoubleAttributeType = 500;
  NSEntityMigrationPolicyError = 134170;
  NSErrorMergePolicyType = 0;
  NSExternalRecordImportError = 134200;
  NSFetchRequestExpressionType = 50;
  NSFetchRequestType = 1;
  NSFetchedResultsChangeDelete = 2;
  NSFetchedResultsChangeInsert = 1;
  NSFetchedResultsChangeMove = 3;
  NSFetchedResultsChangeUpdate = 4;
  NSFloatAttributeType = 600;
  NSInferredMappingModelError = 134190;
  NSInteger16AttributeType = 100;
  NSInteger32AttributeType = 200;
  NSInteger64AttributeType = 300;
  NSMainQueueConcurrencyType = 2;
  NSManagedObjectContextLockingError = 132000;
  NSManagedObjectExternalRelationshipError = 133010;
  NSManagedObjectIDResultType = 1;
  NSManagedObjectMergeError = 133020;
  NSManagedObjectReferentialIntegrityError = 133000;
  NSManagedObjectResultType = 0;
  NSManagedObjectValidationError = 1550;
  NSMergeByPropertyObjectTrumpMergePolicyType = 2;
  NSMergeByPropertyStoreTrumpMergePolicyType = 1;
  NSMigrationCancelledError = 134120;
  NSMigrationError = 134110;
  NSMigrationManagerDestinationStoreError = 134160;
  NSMigrationManagerSourceStoreError = 134150;
  NSMigrationMissingMappingModelError = 134140;
  NSMigrationMissingSourceModelError = 134130;
  NSNoActionDeleteRule = 0;
  NSNullifyDeleteRule = 1;
  NSObjectIDAttributeType = 2000;
  NSOverwriteMergePolicyType = 3;
  NSPersistentStoreCoordinatorLockingError = 132010;
  NSPersistentStoreIncompatibleSchemaError = 134020;
  NSPersistentStoreIncompatibleVersionHashError = 134100;
  NSPersistentStoreIncompleteSaveError = 134040;
  NSPersistentStoreInvalidTypeError = 134000;
  NSPersistentStoreOpenError = 134080;
  NSPersistentStoreOperationError = 134070;
  NSPersistentStoreSaveConflictsError = 134050;
  NSPersistentStoreSaveError = 134030;
  NSPersistentStoreTimeoutError = 134090;
  NSPersistentStoreTypeMismatchError = 134010;
  NSPersistentStoreUnsupportedRequestTypeError = 134091;
  NSPrivateQueueConcurrencyType = 1;
  NSRemoveEntityMappingType = 3;
  NSRollbackMergePolicyType = 4;
  NSSQLiteError = 134180;
  NSSaveRequestType = 2;
  NSSnapshotEventMergePolicy = 64;
  NSSnapshotEventRefresh = 32;
  NSSnapshotEventRollback = 16;
  NSSnapshotEventUndoDeletion = 4;
  NSSnapshotEventUndoInsertion = 2;
  NSSnapshotEventUndoUpdate = 8;
  NSStringAttributeType = 700;
  NSTransformEntityMappingType = 5;
  NSTransformableAttributeType = 1800;
  NSUndefinedAttributeType = 0;
  NSUndefinedEntityMappingType = 0;
  NSValidationDateTooLateError = 1630;
  NSValidationDateTooSoonError = 1640;
  NSValidationInvalidDateError = 1650;
  NSValidationMissingMandatoryPropertyError = 1570;
  NSValidationMultipleErrorsError = 1560;
  NSValidationNumberTooLargeError = 1610;
  NSValidationNumberTooSmallError = 1620;
  NSValidationRelationshipDeniedDeleteError = 1600;
  NSValidationRelationshipExceedsMaximumCountError = 1590;
  NSValidationRelationshipLacksMinimumCountError = 1580;
  NSValidationStringPatternMatchingError = 1680;
  NSValidationStringTooLongError = 1660;
  NSValidationStringTooShortError = 1670;

// ===== Typedefs and structs =====
{$M+}
type
  NSAttributeType = NSUInteger;
  NSDeleteRule = NSUInteger;
  NSEntityMappingType = NSUInteger;
  NSFetchRequestResultType = NSUInteger;
  NSManagedObjectContextConcurrencyType = NSUInteger;
  NSMergePolicyType = NSUInteger;
  NSPersistentStoreRequestType = NSUInteger;
  NSSnapshotEventType = NSUInteger;

// ===== External functions =====

const
  libCoreData = '/System/Library/Frameworks/CoreData.framework/CoreData';


//type
//{$M+}
// ===== Forward declarations =====


// ===== Protocol declarations =====

//  NSFetchedResultsControllerDelegate = interface
//    ['{71C41593-24C6-449B-A7ED-853756073E3C}']
//    procedure controller(controller: NSFetchedResultsController; didChangeObject: Pointer; atIndexPath: NSIndexPath; forChangeType: NSFetchedResultsChangeType; newIndexPath: NSIndexPath); cdecl; overload;
//    procedure controller(controller: NSFetchedResultsController; didChangeSection: Pointer; atIndex: NSUInteger; forChangeType: NSFetchedResultsChangeType); cdecl; overload;
//    function controller(controller: NSFetchedResultsController; sectionIndexTitleForSectionName: NSString): NSString; cdecl; overload;
//    procedure controllerDidChangeContent(controller: NSFetchedResultsController); cdecl;
//    procedure controllerWillChangeContent(controller: NSFetchedResultsController); cdecl;
//  end;
//  NSFetchedResultsSectionInfo = interface
//    ['{778AFBD7-6AF7-47A5-8F1E-666E77464EE2}']
//    function indexTitle: NSString; cdecl;
//    function name: NSString; cdecl;
//    function numberOfObjects: NSUInteger; cdecl;
//    function objects: NSArray; cdecl;
//  end;


type
{$M+}
// ===== Forward declarations =====

  NSMappingModel = interface;
  NSMergeConflict = interface;
  NSMergePolicy = interface;
  NSManagedObjectContext = interface;
  NSManagedObjectID = interface;
  NSManagedObjectModel = interface;
  NSMigrationManager = interface;
  NSPersistentStoreRequest = interface;
  NSPropertyDescription = interface;
  NSPropertyMapping = interface;
  NSPersistentStore = interface;
  NSPersistentStoreCoordinator = interface;
  NSManagedObject = interface;
  NSEntityMapping = interface;
  NSEntityMigrationPolicy = interface;
  NSAtomicStoreCacheNode = interface;
  NSEntityDescription = interface;
  NSFetchRequestExpression = interface;
  NSFetchedResultsController = interface;
  NSIncrementalStoreNode = interface;
  NSAtomicStore = interface;
  NSSaveChangesRequest = interface;
  NSRelationshipDescription = interface;
  NSAttributeDescription = interface;
  NSExpressionDescription = interface;
  NSFetchRequest = interface;
  NSIncrementalStore = interface;
  NSFetchedPropertyDescription = interface;

// ===== Interface declarations =====

  NSMappingModelClass = interface(NSObjectClass)
    ['{63AC4E41-59B6-461E-8BDB-FA718208DFAD}']
    {class} function inferredMappingModelForSourceModel(sourceModel: NSManagedObjectModel; destinationModel: NSManagedObjectModel; error: NSError): Pointer; cdecl;
    {class} function mappingModelFromBundles(bundles: NSArray; forSourceModel: NSManagedObjectModel; destinationModel: NSManagedObjectModel): Pointer; cdecl;
  end;
  NSMappingModel = interface(NSObject)
    ['{EC45C6DE-1BBC-41F5-A24D-744A21052E42}']
    function entityMappings: NSArray; cdecl;
    function entityMappingsByName: NSDictionary; cdecl;
    function initWithContentsOfURL(url: NSURL): Pointer; cdecl;
    procedure setEntityMappings(mappings: NSArray); cdecl;
  end;
  TNSMappingModel = class(TOCGenericImport<NSMappingModelClass, NSMappingModel>)  end;

  NSMergeConflictClass = interface(NSObjectClass)
    ['{0A5B8648-1006-440F-A4BF-AE5E4F3C1036}']
  end;
  NSMergeConflict = interface(NSObject)
    ['{DF228EDB-F4EA-4FFB-9036-40C8DD27B7E1}']
    function cachedSnapshot: NSDictionary; cdecl;
    function initWithSource(srcObject: NSManagedObject; newVersion: NSUInteger; oldVersion: NSUInteger; cachedSnapshot: NSDictionary; persistedSnapshot: NSDictionary): Pointer; cdecl;
    function newVersionNumber: NSUInteger; cdecl;
    function objectSnapshot: NSDictionary; cdecl;
    function oldVersionNumber: NSUInteger; cdecl;
    function persistedSnapshot: NSDictionary; cdecl;
    function sourceObject: NSManagedObject; cdecl;
  end;
  TNSMergeConflict = class(TOCGenericImport<NSMergeConflictClass, NSMergeConflict>)  end;

  NSMergePolicyClass = interface(NSObjectClass)
    ['{1A714DF9-B3AD-4A0E-A4ED-34EB3E696D97}']
  end;
  NSMergePolicy = interface(NSObject)
    ['{6DE4FF12-0F6B-40FB-B5B7-EA78CCCA9937}']
    function initWithMergeType(ty: NSMergePolicyType): Pointer; cdecl;
    function mergeType: NSMergePolicyType; cdecl;
    function resolveConflicts(list: NSArray; error: NSError): Boolean; cdecl;
  end;
  TNSMergePolicy = class(TOCGenericImport<NSMergePolicyClass, NSMergePolicy>)  end;

  NSManagedObjectContextClass = interface(NSObjectClass)
    ['{4B3C0627-2417-4ABF-95F0-E9B1CF5C2512}']
  end;
  NSManagedObjectContext = interface(NSObject)
    ['{8D71FCBA-F1CB-4055-B373-83AFC5EC3EAE}']
    procedure assignObject(object_: Pointer; toPersistentStore: NSPersistentStore); cdecl;
    function concurrencyType: NSManagedObjectContextConcurrencyType; cdecl;
    function countForFetchRequest(request: NSFetchRequest; error: NSError): NSUInteger; cdecl;
    procedure deleteObject(object_: NSManagedObject); cdecl;
    function deletedObjects: NSSet; cdecl;
    procedure detectConflictsForObject(object_: NSManagedObject); cdecl;
    function executeFetchRequest(request: NSFetchRequest; error: NSError): NSArray; cdecl;
    function existingObjectWithID(objectID: NSManagedObjectID; error: NSError): NSManagedObject; cdecl;
    function hasChanges: Boolean; cdecl;
    function initWithConcurrencyType(ct: NSManagedObjectContextConcurrencyType): Pointer; cdecl;
    procedure insertObject(object_: NSManagedObject); cdecl;
    function insertedObjects: NSSet; cdecl;
    procedure lock; cdecl;
    procedure mergeChangesFromContextDidSaveNotification(notification: NSNotification); cdecl;
    function mergePolicy: Pointer; cdecl;
    function objectRegisteredForID(objectID: NSManagedObjectID): NSManagedObject; cdecl;
    function objectWithID(objectID: NSManagedObjectID): NSManagedObject; cdecl;
    procedure observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer); cdecl;
    function obtainPermanentIDsForObjects(objects: NSArray; error: NSError): Boolean; cdecl;
    function parentContext: NSManagedObjectContext; cdecl;
    function persistentStoreCoordinator: NSPersistentStoreCoordinator; cdecl;
    procedure processPendingChanges; cdecl;
    function propagatesDeletesAtEndOfEvent: Boolean; cdecl;
    procedure redo; cdecl;
    procedure refreshObject(object_: NSManagedObject; mergeChanges: Boolean); cdecl;
    function registeredObjects: NSSet; cdecl;
    procedure reset; cdecl;
    function retainsRegisteredObjects: Boolean; cdecl;
    procedure rollback; cdecl;
    function save(error: NSError): Boolean; cdecl;
    procedure setMergePolicy(mergePolicy: Pointer); cdecl;
    procedure setParentContext(parent: NSManagedObjectContext); cdecl;
    procedure setPersistentStoreCoordinator(coordinator: NSPersistentStoreCoordinator); cdecl;
    procedure setPropagatesDeletesAtEndOfEvent(flag: Boolean); cdecl;
    procedure setRetainsRegisteredObjects(flag: Boolean); cdecl;
    procedure setStalenessInterval(expiration: NSTimeInterval); cdecl;
    procedure setUndoManager(undoManager: NSUndoManager); cdecl;
    function stalenessInterval: NSTimeInterval; cdecl;
    function tryLock: Boolean; cdecl;
    procedure undo; cdecl;
    function undoManager: NSUndoManager; cdecl;
    procedure unlock; cdecl;
    function updatedObjects: NSSet; cdecl;
    function userInfo: NSMutableDictionary; cdecl;
  end;
  TNSManagedObjectContext = class(TOCGenericImport<NSManagedObjectContextClass, NSManagedObjectContext>)  end;

  NSManagedObjectIDClass = interface(NSObjectClass)
    ['{2E268789-1589-4EFF-9304-B8F03F82E8C9}']
  end;
  NSManagedObjectID = interface(NSObject)
    ['{85D42F36-1371-4348-A5E4-FD44793CB30C}']
    function URIRepresentation: NSURL; cdecl;
    function entity: NSEntityDescription; cdecl;
    function isTemporaryID: Boolean; cdecl;
    function persistentStore: NSPersistentStore; cdecl;
  end;
  TNSManagedObjectID = class(TOCGenericImport<NSManagedObjectIDClass, NSManagedObjectID>)  end;

  NSManagedObjectModelClass = interface(NSObjectClass)
    ['{E5215F0A-5415-40AA-A25F-F6AA55E163E9}']
    {class} function mergedModelFromBundles(bundles: NSArray): Pointer; cdecl; overload;
    {class} function mergedModelFromBundles(bundles: NSArray; forStoreMetadata: NSDictionary): Pointer; cdecl; overload;
    {class} function modelByMergingModels(models: NSArray): Pointer; cdecl; overload;
    {class} function modelByMergingModels(models: NSArray; forStoreMetadata: NSDictionary): Pointer; cdecl; overload;
  end;
  NSManagedObjectModel = interface(NSObject)
    ['{BC685AB5-7951-44A5-A182-A3170A7868A4}']
    function configurations: NSArray; cdecl;
    function entities: NSArray; cdecl;
    function entitiesByName: NSDictionary; cdecl;
    function entitiesForConfiguration(configuration: NSString): NSArray; cdecl;
    function entityVersionHashesByName: NSDictionary; cdecl;
    function fetchRequestFromTemplateWithName(name: NSString; substitutionVariables: NSDictionary): NSFetchRequest; cdecl;
    function fetchRequestTemplateForName(name: NSString): NSFetchRequest; cdecl;
    function fetchRequestTemplatesByName: NSDictionary; cdecl;
    function init: Pointer; cdecl;
    function initWithContentsOfURL(url: NSURL): Pointer; cdecl;
    function isConfiguration(configuration: NSString; compatibleWithStoreMetadata: NSDictionary): Boolean; cdecl;
    function localizationDictionary: NSDictionary; cdecl;
    procedure setEntities(entities: NSArray); cdecl; overload;
    procedure setEntities(entities: NSArray; forConfiguration: NSString); cdecl; overload;
    procedure setFetchRequestTemplate(fetchRequestTemplate: NSFetchRequest; forName: NSString); cdecl;
    procedure setLocalizationDictionary(localizationDictionary: NSDictionary); cdecl;
    procedure setVersionIdentifiers(identifiers: NSSet); cdecl;
    function versionIdentifiers: NSSet; cdecl;
  end;
  TNSManagedObjectModel = class(TOCGenericImport<NSManagedObjectModelClass, NSManagedObjectModel>)  end;

  NSMigrationManagerClass = interface(NSObjectClass)
    ['{05D0A670-C230-424E-8D79-AF92E63376DB}']
  end;
  NSMigrationManager = interface(NSObject)
    ['{36F93ECB-8CE9-4796-941A-48C2B9DD8E3F}']
    procedure associateSourceInstance(sourceInstance: NSManagedObject; withDestinationInstance: NSManagedObject; forEntityMapping: NSEntityMapping); cdecl;
    procedure cancelMigrationWithError(error: NSError); cdecl;
    function currentEntityMapping: NSEntityMapping; cdecl;
    function destinationContext: NSManagedObjectContext; cdecl;
    function destinationEntityForEntityMapping(mEntity: NSEntityMapping): NSEntityDescription; cdecl;
    function destinationInstancesForEntityMappingNamed(mappingName: NSString; sourceInstances: NSArray): NSArray; cdecl;
    function destinationModel: NSManagedObjectModel; cdecl;
    function initWithSourceModel(sourceModel: NSManagedObjectModel; destinationModel: NSManagedObjectModel): Pointer; cdecl;
    function mappingModel: NSMappingModel; cdecl;
    function migrationProgress: Single; cdecl;
    procedure reset; cdecl;
    procedure setUserInfo(dict: NSDictionary); cdecl;
    procedure setUsesStoreSpecificMigrationManager(flag: Boolean); cdecl;
    function sourceContext: NSManagedObjectContext; cdecl;
    function sourceEntityForEntityMapping(mEntity: NSEntityMapping): NSEntityDescription; cdecl;
    function sourceInstancesForEntityMappingNamed(mappingName: NSString; destinationInstances: NSArray): NSArray; cdecl;
    function sourceModel: NSManagedObjectModel; cdecl;
    function userInfo: NSDictionary; cdecl;
    function usesStoreSpecificMigrationManager: Boolean; cdecl;
  end;
  TNSMigrationManager = class(TOCGenericImport<NSMigrationManagerClass, NSMigrationManager>)  end;

  NSPersistentStoreRequestClass = interface(NSObjectClass)
    ['{34D96963-F1A1-4CBC-9ABF-494601DE3AF9}']
  end;
  NSPersistentStoreRequest = interface(NSObject)
    ['{C606EE35-8921-4F28-BCAD-21B7E9B0FCD4}']
    function affectedStores: NSArray; cdecl;
    function requestType: NSPersistentStoreRequestType; cdecl;
    procedure setAffectedStores(stores: NSArray); cdecl;
  end;
  TNSPersistentStoreRequest = class(TOCGenericImport<NSPersistentStoreRequestClass, NSPersistentStoreRequest>)  end;

  NSPropertyDescriptionClass = interface(NSObjectClass)
    ['{8E659BEE-6D35-4FB4-833A-ECE177C6F88D}']
  end;
  NSPropertyDescription = interface(NSObject)
    ['{FEA1210A-25AB-4B5B-B94E-1A3B8323C064}']
    function entity: NSEntityDescription; cdecl;
    function isIndexed: Boolean; cdecl;
    function isIndexedBySpotlight: Boolean; cdecl;
    function isOptional: Boolean; cdecl;
    function isStoredInExternalRecord: Boolean; cdecl;
    function isTransient: Boolean; cdecl;
    function name: NSString; cdecl;
    function renamingIdentifier: NSString; cdecl;
    procedure setIndexed(flag: Boolean); cdecl;
    procedure setIndexedBySpotlight(flag: Boolean); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setOptional(flag: Boolean); cdecl;
    procedure setRenamingIdentifier(value: NSString); cdecl;
    procedure setStoredInExternalRecord(flag: Boolean); cdecl;
    procedure setTransient(flag: Boolean); cdecl;
    procedure setUserInfo(dictionary: NSDictionary); cdecl;
    procedure setValidationPredicates(validationPredicates: NSArray; withValidationWarnings: NSArray); cdecl;
    procedure setVersionHashModifier(modifierString: NSString); cdecl;
    function userInfo: NSDictionary; cdecl;
    function validationPredicates: NSArray; cdecl;
    function validationWarnings: NSArray; cdecl;
    function versionHash: NSData; cdecl;
    function versionHashModifier: NSString; cdecl;
  end;
  TNSPropertyDescription = class(TOCGenericImport<NSPropertyDescriptionClass, NSPropertyDescription>)  end;

  NSPropertyMappingClass = interface(NSObjectClass)
    ['{4AE8DE07-7968-4FF5-8EDA-9E909DE61247}']
  end;
  NSPropertyMapping = interface(NSObject)
    ['{DCBFEBF6-C554-4F2D-905E-E8FB44CAEE79}']
    function name: NSString; cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    procedure setValueExpression(expression: NSExpression); cdecl;
    function userInfo: NSDictionary; cdecl;
    function valueExpression: NSExpression; cdecl;
  end;
  TNSPropertyMapping = class(TOCGenericImport<NSPropertyMappingClass, NSPropertyMapping>)  end;

  NSPersistentStoreClass = interface(NSObjectClass)
    ['{F9399137-998A-4D9E-9FBD-8BAD36B45451}']
    {class} function metadataForPersistentStoreWithURL(url: NSURL; error: NSError): NSDictionary; cdecl;
    {class} function setMetadata(metadata: NSDictionary; forPersistentStoreWithURL: NSURL; error: NSError): Boolean; cdecl; overload;
  end;
  NSPersistentStore = interface(NSObject)
    ['{AE92EC4A-E3F3-47E3-9BE5-93C67763F776}']
    function URL: NSURL; cdecl;
    function configurationName: NSString; cdecl;
    procedure didAddToPersistentStoreCoordinator(coordinator: NSPersistentStoreCoordinator); cdecl;
    function identifier: NSString; cdecl;
    function initWithPersistentStoreCoordinator(root: NSPersistentStoreCoordinator; configurationName: NSString; URL: NSURL; options: NSDictionary): Pointer; cdecl;
    function isReadOnly: Boolean; cdecl;
    function loadMetadata(error: NSError): Boolean; cdecl;
    function metadata: NSDictionary; cdecl;
    function options: NSDictionary; cdecl;
    function persistentStoreCoordinator: NSPersistentStoreCoordinator; cdecl;
    procedure setIdentifier(identifier: NSString); cdecl;
    procedure setMetadata(storeMetadata: NSDictionary); cdecl; overload;
    procedure setReadOnly(flag: Boolean); cdecl;
    procedure setURL(url: NSURL); cdecl;
    procedure willRemoveFromPersistentStoreCoordinator(coordinator: NSPersistentStoreCoordinator); cdecl;
  end;
  TNSPersistentStore = class(TOCGenericImport<NSPersistentStoreClass, NSPersistentStore>)  end;

  NSPersistentStoreCoordinatorClass = interface(NSObjectClass)
    ['{DE87F288-F2D5-4F36-A72D-85C91532B4FF}']
    {class} function metadataForPersistentStoreOfType(storeType: NSString; URL: NSURL; error: NSError): NSDictionary; cdecl;
    {class} procedure registerStoreClass(storeClass: Pointer; forStoreType: NSString); cdecl;
    {class} function registeredStoreTypes: NSDictionary; cdecl;
    {class} function setMetadata(metadata: NSDictionary; forPersistentStoreOfType: NSString; URL: NSURL; error: NSError): Boolean; cdecl; overload;
  end;
  NSPersistentStoreCoordinator = interface(NSObject)
    ['{C1AE026B-86D6-40F1-9884-76E37F98E971}']
    function URLForPersistentStore(store: NSPersistentStore): NSURL; cdecl;
    function addPersistentStoreWithType(storeType: NSString; configuration: NSString; URL: NSURL; options: NSDictionary; error: NSError): NSPersistentStore; cdecl;
    function executeRequest(request: NSPersistentStoreRequest; withContext: NSManagedObjectContext; error: NSError): Pointer; cdecl;
    function initWithManagedObjectModel(model: NSManagedObjectModel): Pointer; cdecl;
    procedure lock; cdecl;
    function managedObjectIDForURIRepresentation(url: NSURL): NSManagedObjectID; cdecl;
    function managedObjectModel: NSManagedObjectModel; cdecl;
    function metadataForPersistentStore(store: NSPersistentStore): NSDictionary; cdecl;
    function migratePersistentStore(store: NSPersistentStore; toURL: NSURL; options: NSDictionary; withType: NSString; error: NSError): NSPersistentStore; cdecl;
    function persistentStoreForURL(URL: NSURL): NSPersistentStore; cdecl;
    function persistentStores: NSArray; cdecl;
    function removePersistentStore(store: NSPersistentStore; error: NSError): Boolean; cdecl;
    procedure setMetadata(metadata: NSDictionary; forPersistentStore: NSPersistentStore); cdecl; overload;
    function setURL(url: NSURL; forPersistentStore: NSPersistentStore): Boolean; cdecl;
    function tryLock: Boolean; cdecl;
    procedure unlock; cdecl;
  end;
  TNSPersistentStoreCoordinator = class(TOCGenericImport<NSPersistentStoreCoordinatorClass, NSPersistentStoreCoordinator>)  end;

  NSManagedObjectClass = interface(NSObjectClass)
    ['{25B84FEC-04DB-4F87-8E77-B6BAD4CFD779}']
    {class} function contextShouldIgnoreUnmodeledPropertyChanges: Boolean; cdecl;
  end;
  NSManagedObject = interface(NSObject)
    ['{4B980CDA-4853-49C4-93F3-D8F585697011}']
    procedure awakeFromFetch; cdecl;
    procedure awakeFromInsert; cdecl;
    procedure awakeFromSnapshotEvents(flags: NSSnapshotEventType); cdecl;
    function changedValues: NSDictionary; cdecl;
    function changedValuesForCurrentEvent: NSDictionary; cdecl;
    function committedValuesForKeys(keys: NSArray): NSDictionary; cdecl;
    procedure didAccessValueForKey(key: NSString); cdecl;
    procedure didChangeValueForKey(key: NSString); cdecl; overload;
    procedure didChangeValueForKey(inKey: NSString; withSetMutation: NSKeyValueSetMutationKind; usingObjects: NSSet); cdecl; overload;
    procedure didSave; cdecl;
    procedure didTurnIntoFault; cdecl;
    function entity: NSEntityDescription; cdecl;
    function faultingState: NSUInteger; cdecl;
    function hasChanges: Boolean; cdecl;
    function hasFaultForRelationshipNamed(key: NSString): Boolean; cdecl;
    function initWithEntity(entity: NSEntityDescription; insertIntoManagedObjectContext: NSManagedObjectContext): Pointer; cdecl;
    function isDeleted: Boolean; cdecl;
    function isFault: Boolean; cdecl;
    function isInserted: Boolean; cdecl;
    function isUpdated: Boolean; cdecl;
    function managedObjectContext: NSManagedObjectContext; cdecl;
    function objectID: NSManagedObjectID; cdecl;
    function observationInfo: Pointer; cdecl;
    procedure prepareForDeletion; cdecl;
    function primitiveValueForKey(key: NSString): Pointer; cdecl;
    procedure setObservationInfo(inObservationInfo: Pointer); cdecl;
    procedure setPrimitiveValue(value: Pointer; forKey: NSString); cdecl;
    procedure setValue(value: Pointer; forKey: NSString); cdecl;
    function validateForDelete(error: NSError): Boolean; cdecl;
    function validateForInsert(error: NSError): Boolean; cdecl;
    function validateForUpdate(error: NSError): Boolean; cdecl;
    function validateValue(value: Pointer; forKey: NSString; error: NSError): Boolean; cdecl;
    function valueForKey(key: NSString): Pointer; cdecl;
    procedure willAccessValueForKey(key: NSString); cdecl;
    procedure willChangeValueForKey(key: NSString); cdecl; overload;
    procedure willChangeValueForKey(inKey: NSString; withSetMutation: NSKeyValueSetMutationKind; usingObjects: NSSet); cdecl; overload;
    procedure willSave; cdecl;
    procedure willTurnIntoFault; cdecl;
  end;
  TNSManagedObject = class(TOCGenericImport<NSManagedObjectClass, NSManagedObject>)  end;

  NSEntityMappingClass = interface(NSObjectClass)
    ['{6B39438F-7DCE-47BD-AFA3-63A3A5141AE8}']
  end;
  NSEntityMapping = interface(NSObject)
    ['{77B2622D-863F-44C0-A133-EE601C64C30D}']
    function attributeMappings: NSArray; cdecl;
    function destinationEntityName: NSString; cdecl;
    function destinationEntityVersionHash: NSData; cdecl;
    function entityMigrationPolicyClassName: NSString; cdecl;
    function mappingType: NSEntityMappingType; cdecl;
    function name: NSString; cdecl;
    function relationshipMappings: NSArray; cdecl;
    procedure setAttributeMappings(mappings: NSArray); cdecl;
    procedure setDestinationEntityName(name: NSString); cdecl;
    procedure setDestinationEntityVersionHash(vhash: NSData); cdecl;
    procedure setEntityMigrationPolicyClassName(name: NSString); cdecl;
    procedure setMappingType(type_: NSEntityMappingType); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setRelationshipMappings(mappings: NSArray); cdecl;
    procedure setSourceEntityName(name: NSString); cdecl;
    procedure setSourceEntityVersionHash(vhash: NSData); cdecl;
    procedure setSourceExpression(source: NSExpression); cdecl;
    procedure setUserInfo(dict: NSDictionary); cdecl;
    function sourceEntityName: NSString; cdecl;
    function sourceEntityVersionHash: NSData; cdecl;
    function sourceExpression: NSExpression; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TNSEntityMapping = class(TOCGenericImport<NSEntityMappingClass, NSEntityMapping>)  end;

  NSEntityMigrationPolicyClass = interface(NSObjectClass)
    ['{24AE98EA-97C8-4AA2-BE7F-9F67D8177F8E}']
  end;
  NSEntityMigrationPolicy = interface(NSObject)
    ['{D0E1C6D4-2750-4F30-96A6-01FE1646B106}']
    function beginEntityMapping(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSError): Boolean; cdecl;
    function createDestinationInstancesForSourceInstance(sInstance: NSManagedObject; entityMapping: NSEntityMapping; manager: NSMigrationManager; error: NSError): Boolean; cdecl;
    function createRelationshipsForDestinationInstance(dInstance: NSManagedObject; entityMapping: NSEntityMapping; manager: NSMigrationManager; error: NSError): Boolean; cdecl;
    function endEntityMapping(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSError): Boolean; cdecl;
    function endInstanceCreationForEntityMapping(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSError): Boolean; cdecl;
    function endRelationshipCreationForEntityMapping(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSError): Boolean; cdecl;
    function performCustomValidationForEntityMapping(mapping: NSEntityMapping; manager: NSMigrationManager; error: NSError): Boolean; cdecl;
  end;
  TNSEntityMigrationPolicy = class(TOCGenericImport<NSEntityMigrationPolicyClass, NSEntityMigrationPolicy>)  end;

  NSAtomicStoreCacheNodeClass = interface(NSObjectClass)
    ['{EBDD6FE0-5B68-4AC9-AB60-24775C4D844D}']
  end;
  NSAtomicStoreCacheNode = interface(NSObject)
    ['{7D98386A-FC49-485A-8634-4745ADEF18F6}']
    function initWithObjectID(moid: NSManagedObjectID): Pointer; cdecl;
    function objectID: NSManagedObjectID; cdecl;
    function propertyCache: NSMutableDictionary; cdecl;
    procedure setPropertyCache(propertyCache: NSMutableDictionary); cdecl;
    procedure setValue(value: Pointer; forKey: NSString); cdecl;
    function valueForKey(key: NSString): Pointer; cdecl;
  end;
  TNSAtomicStoreCacheNode = class(TOCGenericImport<NSAtomicStoreCacheNodeClass, NSAtomicStoreCacheNode>)  end;

  NSEntityDescriptionClass = interface(NSObjectClass)
    ['{C7FE5391-A70E-45F7-BFCA-BEAF0F15481D}']
    {class} function entityForName(entityName: NSString; inManagedObjectContext: NSManagedObjectContext): Pointer; cdecl;
    {class} function insertNewObjectForEntityForName(entityName: NSString; inManagedObjectContext: NSManagedObjectContext): Pointer; cdecl;
  end;
  NSEntityDescription = interface(NSObject)
    ['{76DD39A4-1905-4DDE-9021-C2F14C89E1BB}']
    function attributesByName: NSDictionary; cdecl;
    function compoundIndexes: NSArray; cdecl;
    function isAbstract: Boolean; cdecl;
    function isKindOfEntity(entity: NSEntityDescription): Boolean; cdecl;
    function managedObjectClassName: NSString; cdecl;
    function managedObjectModel: NSManagedObjectModel; cdecl;
    function name: NSString; cdecl;
    function properties: NSArray; cdecl;
    function propertiesByName: NSDictionary; cdecl;
    function relationshipsByName: NSDictionary; cdecl;
    function relationshipsWithDestinationEntity(entity: NSEntityDescription): NSArray; cdecl;
    function renamingIdentifier: NSString; cdecl;
    procedure setAbstract(flag: Boolean); cdecl;
    procedure setCompoundIndexes(value: NSArray); cdecl;
    procedure setManagedObjectClassName(name: NSString); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setProperties(properties: NSArray); cdecl;
    procedure setRenamingIdentifier(value: NSString); cdecl;
    procedure setSubentities(array_: NSArray); cdecl;
    procedure setUserInfo(dictionary: NSDictionary); cdecl;
    procedure setVersionHashModifier(modifierString: NSString); cdecl;
    function subentities: NSArray; cdecl;
    function subentitiesByName: NSDictionary; cdecl;
    function superentity: NSEntityDescription; cdecl;
    function userInfo: NSDictionary; cdecl;
    function versionHash: NSData; cdecl;
    function versionHashModifier: NSString; cdecl;
  end;
  TNSEntityDescription = class(TOCGenericImport<NSEntityDescriptionClass, NSEntityDescription>)  end;

  NSFetchRequestExpressionClass = interface(NSExpressionClass)
    ['{330EC7A6-1685-4A35-9F46-769C14C0CC90}']
    {class} function expressionForFetch(fetch: NSExpression; context: NSExpression; countOnly: Boolean): NSExpression; cdecl;
  end;
  NSFetchRequestExpression = interface(NSExpression)
    ['{AE2713C4-6106-4432-8CBC-EE4468602762}']
    function contextExpression: NSExpression; cdecl;
    function isCountOnlyRequest: Boolean; cdecl;
    function requestExpression: NSExpression; cdecl;
  end;
  TNSFetchRequestExpression = class(TOCGenericImport<NSFetchRequestExpressionClass, NSFetchRequestExpression>)  end;

  NSFetchedResultsControllerClass = interface(NSObjectClass)
    ['{B3158917-371A-4DDE-B0F0-BB2BE109BD15}']
    {class} procedure deleteCacheWithName(name: NSString); cdecl;
  end;
  NSFetchedResultsController = interface(NSObject)
    ['{E77C61EA-F5B0-40AA-9F6C-224B27945043}']
    function cacheName: NSString; cdecl;
    function delegate: Pointer; cdecl;
    function fetchRequest: NSFetchRequest; cdecl;
    function fetchedObjects: NSArray; cdecl;
    function indexPathForObject(object_: Pointer): NSIndexPath; cdecl;
    function initWithFetchRequest(fetchRequest: NSFetchRequest; managedObjectContext: NSManagedObjectContext; sectionNameKeyPath: NSString; cacheName: NSString): Pointer; cdecl;
    function managedObjectContext: NSManagedObjectContext; cdecl;
    function objectAtIndexPath(indexPath: NSIndexPath): Pointer; cdecl;
    function performFetch(error: NSError): Boolean; cdecl;
    function sectionForSectionIndexTitle(title: NSString; atIndex: NSInteger): NSInteger; cdecl;
    function sectionIndexTitleForSectionName(sectionName: NSString): NSString; cdecl;
    function sectionIndexTitles: NSArray; cdecl;
    function sectionNameKeyPath: NSString; cdecl;
    function sections: NSArray; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TNSFetchedResultsController = class(TOCGenericImport<NSFetchedResultsControllerClass, NSFetchedResultsController>)  end;

  NSIncrementalStoreNodeClass = interface(NSObjectClass)
    ['{C1A16F62-6B77-4F02-A1D7-E89AD712813A}']
  end;
  NSIncrementalStoreNode = interface(NSObject)
    ['{0054849A-1220-4897-8C7C-4A46AAD113BE}']
    function initWithObjectID(objectID: NSManagedObjectID; withValues: NSDictionary; version: UInt64): Pointer; cdecl;
    function objectID: NSManagedObjectID; cdecl;
    procedure updateWithValues(values: NSDictionary; version: UInt64); cdecl;
    function valueForPropertyDescription(prop: NSPropertyDescription): Pointer; cdecl;
    function version: UInt64; cdecl;
  end;
  TNSIncrementalStoreNode = class(TOCGenericImport<NSIncrementalStoreNodeClass, NSIncrementalStoreNode>)  end;

  NSAtomicStoreClass = interface(NSPersistentStoreClass)
    ['{DEB77ED2-9713-40CD-BE4D-8BE25BAAC95F}']
  end;
  NSAtomicStore = interface(NSPersistentStore)
    ['{87C90CF6-E7D7-4C18-A35E-ED885A53A740}']
    procedure addCacheNodes(cacheNodes: NSSet); cdecl;
    function cacheNodeForObjectID(objectID: NSManagedObjectID): NSAtomicStoreCacheNode; cdecl;
    function cacheNodes: NSSet; cdecl;
    function initWithPersistentStoreCoordinator(coordinator: NSPersistentStoreCoordinator; configurationName: NSString; URL: NSURL; options: NSDictionary): Pointer; cdecl;
    function load(error: NSError): Boolean; cdecl;
    function newCacheNodeForManagedObject(managedObject: NSManagedObject): NSAtomicStoreCacheNode; cdecl;
    function newReferenceObjectForManagedObject(managedObject: NSManagedObject): Pointer; cdecl;
    function objectIDForEntity(entity: NSEntityDescription; referenceObject: Pointer): NSManagedObjectID; cdecl;
    function referenceObjectForObjectID(objectID: NSManagedObjectID): Pointer; cdecl;
    function save(error: NSError): Boolean; cdecl;
    procedure updateCacheNode(node: NSAtomicStoreCacheNode; fromManagedObject: NSManagedObject); cdecl;
    procedure willRemoveCacheNodes(cacheNodes: NSSet); cdecl;
  end;
  TNSAtomicStore = class(TOCGenericImport<NSAtomicStoreClass, NSAtomicStore>)  end;

  NSSaveChangesRequestClass = interface(NSPersistentStoreRequestClass)
    ['{7374668B-52CA-4929-9EB6-CF2D659081FA}']
  end;
  NSSaveChangesRequest = interface(NSPersistentStoreRequest)
    ['{C31BD3D0-DAC0-47A0-92EF-19E040A934EC}']
    function deletedObjects: NSSet; cdecl;
    function initWithInsertedObjects(insertedObjects: NSSet; updatedObjects: NSSet; deletedObjects: NSSet; lockedObjects: NSSet): Pointer; cdecl;
    function insertedObjects: NSSet; cdecl;
    function lockedObjects: NSSet; cdecl;
    function updatedObjects: NSSet; cdecl;
  end;
  TNSSaveChangesRequest = class(TOCGenericImport<NSSaveChangesRequestClass, NSSaveChangesRequest>)  end;

  NSRelationshipDescriptionClass = interface(NSPropertyDescriptionClass)
    ['{C6A58AA6-048C-41A1-9039-5BFEC691CD88}']
  end;
  NSRelationshipDescription = interface(NSPropertyDescription)
    ['{809570AE-1E79-488D-8999-8B7FCBE63A00}']
    function deleteRule: NSDeleteRule; cdecl;
    function destinationEntity: NSEntityDescription; cdecl;
    function inverseRelationship: NSRelationshipDescription; cdecl;
    function isOrdered: Boolean; cdecl;
    function isToMany: Boolean; cdecl;
    function maxCount: NSUInteger; cdecl;
    function minCount: NSUInteger; cdecl;
    procedure setDeleteRule(rule: NSDeleteRule); cdecl;
    procedure setDestinationEntity(entity: NSEntityDescription); cdecl;
    procedure setInverseRelationship(relationship: NSRelationshipDescription); cdecl;
    procedure setMaxCount(maxCount: NSUInteger); cdecl;
    procedure setMinCount(minCount: NSUInteger); cdecl;
    procedure setOrdered(flag: Boolean); cdecl;
    function versionHash: NSData; cdecl;
  end;
  TNSRelationshipDescription = class(TOCGenericImport<NSRelationshipDescriptionClass, NSRelationshipDescription>)  end;

  NSAttributeDescriptionClass = interface(NSPropertyDescriptionClass)
    ['{D4C64F53-2034-4093-B587-4954E50E1E9B}']
  end;
  NSAttributeDescription = interface(NSPropertyDescription)
    ['{E7525400-E928-4C30-9637-D3B5B3C78DEF}']
    function allowsExternalBinaryDataStorage: Boolean; cdecl;
    function attributeType: NSAttributeType; cdecl;
    function attributeValueClassName: NSString; cdecl;
    function defaultValue: Pointer; cdecl;
    procedure setAllowsExternalBinaryDataStorage(flag: Boolean); cdecl;
    procedure setAttributeType(type_: NSAttributeType); cdecl;
    procedure setAttributeValueClassName(className: NSString); cdecl;
    procedure setDefaultValue(value: Pointer); cdecl;
    procedure setValueTransformerName(string_: NSString); cdecl;
    function valueTransformerName: NSString; cdecl;
    function versionHash: NSData; cdecl;
  end;
  TNSAttributeDescription = class(TOCGenericImport<NSAttributeDescriptionClass, NSAttributeDescription>)  end;

  NSExpressionDescriptionClass = interface(NSPropertyDescriptionClass)
    ['{43D4D384-9FC3-40D6-B02E-35F646D82FF8}']
  end;
  NSExpressionDescription = interface(NSPropertyDescription)
    ['{B2E09F7C-396B-4B25-92A9-0527C32F1CB2}']
    function expression: NSExpression; cdecl;
    function expressionResultType: NSAttributeType; cdecl;
    procedure setExpression(expression: NSExpression); cdecl;
    procedure setExpressionResultType(type_: NSAttributeType); cdecl;
  end;
  TNSExpressionDescription = class(TOCGenericImport<NSExpressionDescriptionClass, NSExpressionDescription>)  end;

  NSFetchRequestClass = interface(NSPersistentStoreRequestClass)
    ['{BCB5CC13-82B7-41B7-8729-F68BE8FFB513}']
    {class} function fetchRequestWithEntityName(entityName: NSString): Pointer; cdecl;
  end;
  NSFetchRequest = interface(NSPersistentStoreRequest)
    ['{A9C2B82B-5C6C-4CD5-B6DF-3F18CEC79A95}']
    function affectedStores: NSArray; cdecl;
    function entity: NSEntityDescription; cdecl;
    function entityName: NSString; cdecl;
    function fetchBatchSize: NSUInteger; cdecl;
    function fetchLimit: NSUInteger; cdecl;
    function fetchOffset: NSUInteger; cdecl;
    function havingPredicate: NSPredicate; cdecl;
    function includesPendingChanges: Boolean; cdecl;
    function includesPropertyValues: Boolean; cdecl;
    function includesSubentities: Boolean; cdecl;
    function init: Pointer; cdecl;
    function initWithEntityName(entityName: NSString): Pointer; cdecl;
    function predicate: NSPredicate; cdecl;
    function propertiesToFetch: NSArray; cdecl;
    function propertiesToGroupBy: NSArray; cdecl;
    function relationshipKeyPathsForPrefetching: NSArray; cdecl;
    function resultType: NSFetchRequestResultType; cdecl;
    function returnsDistinctResults: Boolean; cdecl;
    function returnsObjectsAsFaults: Boolean; cdecl;
    procedure setAffectedStores(stores: NSArray); cdecl;
    procedure setEntity(entity: NSEntityDescription); cdecl;
    procedure setFetchBatchSize(bsize: NSUInteger); cdecl;
    procedure setFetchLimit(limit: NSUInteger); cdecl;
    procedure setFetchOffset(offset: NSUInteger); cdecl;
    procedure setHavingPredicate(predicate: NSPredicate); cdecl;
    procedure setIncludesPendingChanges(flag: Boolean); cdecl;
    procedure setIncludesPropertyValues(yesNo: Boolean); cdecl;
    procedure setIncludesSubentities(yesNo: Boolean); cdecl;
    procedure setPredicate(predicate: NSPredicate); cdecl;
    procedure setPropertiesToFetch(values: NSArray); cdecl;
    procedure setPropertiesToGroupBy(array_: NSArray); cdecl;
    procedure setRelationshipKeyPathsForPrefetching(keys: NSArray); cdecl;
    procedure setResultType(type_: NSFetchRequestResultType); cdecl;
    procedure setReturnsDistinctResults(flag: Boolean); cdecl;
    procedure setReturnsObjectsAsFaults(yesNo: Boolean); cdecl;
    procedure setShouldRefreshRefetchedObjects(flag: Boolean); cdecl;
    procedure setSortDescriptors(sortDescriptors: NSArray); cdecl;
    function shouldRefreshRefetchedObjects: Boolean; cdecl;
    function sortDescriptors: NSArray; cdecl;
  end;
  TNSFetchRequest = class(TOCGenericImport<NSFetchRequestClass, NSFetchRequest>)  end;

  NSIncrementalStoreClass = interface(NSPersistentStoreClass)
    ['{7930BC8D-E524-4CF2-A5F9-9DAC1EDF8CE4}']
    {class} function identifierForNewStoreAtURL(storeURL: NSURL): Pointer; cdecl;
  end;
  NSIncrementalStore = interface(NSPersistentStore)
    ['{F44000C5-5E2A-4331-B6D2-6056DD2E0F7D}']
    function executeRequest(request: NSPersistentStoreRequest; withContext: NSManagedObjectContext; error: NSError): Pointer; cdecl;
    function loadMetadata(error: NSError): Boolean; cdecl;
    procedure managedObjectContextDidRegisterObjectsWithIDs(objectIDs: NSArray); cdecl;
    procedure managedObjectContextDidUnregisterObjectsWithIDs(objectIDs: NSArray); cdecl;
    function newObjectIDForEntity(entity: NSEntityDescription; referenceObject: Pointer): NSManagedObjectID; cdecl;
    function newValueForRelationship(relationship: NSRelationshipDescription; forObjectWithID: NSManagedObjectID; withContext: NSManagedObjectContext; error: NSError): Pointer; cdecl;
    function newValuesForObjectWithID(objectID: NSManagedObjectID; withContext: NSManagedObjectContext; error: NSError): NSIncrementalStoreNode; cdecl;
    function obtainPermanentIDsForObjects(array_: NSArray; error: NSError): NSArray; cdecl;
    function referenceObjectForObjectID(objectID: NSManagedObjectID): Pointer; cdecl;
  end;
  TNSIncrementalStore = class(TOCGenericImport<NSIncrementalStoreClass, NSIncrementalStore>)  end;

  NSFetchedPropertyDescriptionClass = interface(NSPropertyDescriptionClass)
    ['{4E7F3A8B-93BA-4906-BC29-1C49A893F4A1}']
  end;
  NSFetchedPropertyDescription = interface(NSPropertyDescription)
    ['{5108171A-1F8A-4C6C-B7C1-9C41A613E283}']
    function fetchRequest: NSFetchRequest; cdecl;
    procedure setFetchRequest(fetchRequest: NSFetchRequest); cdecl;
  end;
  TNSFetchedPropertyDescription = class(TOCGenericImport<NSFetchedPropertyDescriptionClass, NSFetchedPropertyDescription>)  end;



implementation

end.
