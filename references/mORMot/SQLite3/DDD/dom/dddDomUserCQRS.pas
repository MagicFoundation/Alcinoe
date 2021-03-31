/// shared DDD Domains: User CQRS Repository interfaces
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddDomUserCQRS;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SynCommons,
  SysUtils,
  Classes,
  mORMot,
  mORMotDDD,
  dddDomUserTypes;

type
  /// defines an abstract CQRS Repository for Reading TUser Aggregate Roots
  // - this interface allows only read access to the Aggregate: see
  // IDomUserCommand to modify the content
  // - you could use SelectByLogonName, SelectByLastName or SelectByEmailValidation
  // methods to initialize a request, then call Get, GetAll or GetNext to retrieve
  // the actual matching Aggregate Roots
  IDomUserQuery = interface(ICQRSService)
    ['{198C01D6-5189-4B74-AAF4-C322237D7D53}']
    /// would select a single TUser from its logon name
    // - then use Get() method to retrieve its content
    function SelectByLogonName(const aLogonName: RawUTF8): TCQRSResult;
    /// would select one or several TUser from their email validation state
    // - then use GetCount, GetAll() or GetNext() methods to retrieve the items
    function SelectByEmailValidation(aValidationState: TDomUserEmailValidation): TCQRSResult;
    /// would select one or several TUser from their last name
    // - will search for a full matching name, unless aStartWith is TRUE so that
    // it would search for the beginning characters
    // - then use GetCount, GetAll() or GetNext() methods to retrieve the items
    function SelectByLastName(const aName: TLastName; aStartWith: boolean): TCQRSResult;
    /// would select all TUser instances
    // - you should not use this search criteria, since it may return a huge
    // number of values
    // - then use GetCount, GetAll() or GetNext() methods to retrieve the items
    function SelectAll: TCQRSResult;
    /// retrieve a single TUser
    function Get(out aAggregate: TUser): TCQRSResult;
    /// retrieve all matching TUser instances
    // - the caller should release all returned TUser by calling
    // ! ObjArrayClear(aAggregates);
    function GetAll(out aAggregates: TUserObjArray): TCQRSResult;
    /// retrieve the next matching TUser instances
    // - returns cqrsNoMoreData if there is no more pending data
    function GetNext(out aAggregate: TUser): TCQRSResult;
    /// retrieve how many TUser instances do match the selection
    function GetCount: integer;
    /// retrieve how many TUser have their email validated
    function HowManyValidatedEmail: integer;
  end;

  /// defines an abstract CQRS Repository for Writing TUser Aggregate Roots
  // - would implement a dual-phase commit to change TUser content
  // - first phase consists in calling Add, Update, Delete or DeleteAll methods
  // which would call the registered validators on the supplied content
  // - you can call Add, Update, Delete or DeleteAll methods several times,
  // so that several write operations will be recorded for the TUser
  // - during the first phase, nothing is actually written to the persistence
  // storage itself (which may be a RDBMS or a NoSQL engine)
  // - then the second phase would take place when the Commit method would
  // be executed, which would save all prepared content to the actual storage
  // engine (e.g. using a transaction via a BATCH process if implemented by
  // mORMot's ORM, via TInfraRepoUser as defined in dddInfraRepoUser) 
  IDomUserCommand = interface(IDomUserQuery)
    ['{D345854F-7337-4006-B324-5D635FBED312}']
    /// persist a new TUser aggregate
    function Add(const aAggregate: TUser): TCQRSResult;
    /// update an existing TUser aggregate
    // - the existing content should have been retrieved by a previous Select*
    // method, e.g. IDomUserQuery.SelectByLogonName
    function Update(const aUpdatedAggregate: TUser): TCQRSResult;
    /// erase an existing TUser aggregate
    // - the existing content should have been retrieved by a previous Select*
    // method, e.g. IDomUserQuery.SelectByLogonName
    function Delete: TCQRSResult;
    /// erase existing TUser aggregate, matching a
    // - the existing content should have been retrieved by a previous Select*
    // method, e.g. IDomUserQuery.SelectByLogonName: a plain DeleteAll call
    // with no prious Select* would return an error
    function DeleteAll: TCQRSResult;
    /// write all pending changes prepared by Add/Update/Delete methods
    // - following the dual-phase pattern, nothing would be written to the
    // actual persistence store unless this method is actually called
    function Commit: TCQRSResult;
    /// flush any pending changes prepared by Add/Update/Delete methods
    // - is the same as releasing the actual IDomUserCommand instance and
    // creating a new one
    function Rollback: TCQRSResult;
  end;

  
implementation

initialization
  TInterfaceFactory.RegisterInterfaces(
    [TypeInfo(IDomUserQuery),TypeInfo(IDomUserCommand)]);
end.
