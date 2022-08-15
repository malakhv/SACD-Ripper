/* *
 * Copyright (C) 2013 Mikhail Malakhov <malakhv@live.ru>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * */

package com.malakhv.sripper.sacd;

/**
 * The class describes SACD album information.
 * @author Mikhail.Malakhov
 * */
public final class AlbumInfo extends BaseInfo {

    /** The size of set. Can be {@code null}. */
    public String set = null;

    /** {@inheritDoc} */
    @Override
    public void clear() {
        super.clear(); set = null;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return "AlbumInfo {" + super.toString() + ", set=" + set + "}";
    }
}